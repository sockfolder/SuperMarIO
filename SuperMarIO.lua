-- MarI/O by SethBling
-- Feel free to use this code, but please do not redistribute it.
-- Intended for use with the BizHawk emulator and Super Mario World or Super Mario Bros. ROM.
 
-- Modified by Sockfolder

---========================================---
--- LIBRARY
---========================================---

----------------------------------------------
-- Pickle.lua
-- A table serialization utility for lua
-- Steve Dekorte, http://www.dekorte.com, Apr 2000
-- Freeware
----------------------------------------------

function pickle(t)
  return Pickle:clone():pickle_(t)
end

Pickle = {
  clone = function (t) local nt={}; for i, v in pairs(t) do nt[i]=v end return nt end 
}

function Pickle:pickle_(root)
  if type(root) ~= "table" then 
    error("can only pickle tables, not ".. type(root).."s")
  end
  self._tableToRef = {}
  self._refToTable = {}
  local savecount = 0
  self:ref_(root)
  local s = ""

  while table.getn(self._refToTable) > savecount do
    savecount = savecount + 1
    local t = self._refToTable[savecount]
    s = s.."{\n"
    for i, v in pairs(t) do
        s = string.format("%s[%s]=%s,\n", s, self:value_(i), self:value_(v))
    end
    s = s.."},\n"
  end

  return string.format("{%s}", s)
end

function Pickle:value_(v)
  local vtype = type(v)
  if     vtype == "string" then return string.format("%q", v)
  elseif vtype == "number" then return v
  elseif vtype == "boolean" then return tostring(v)
  elseif vtype == "table" then return "{"..self:ref_(v).."}"
  else --error("pickle a "..type(v).." is not supported")
  end  
end

function Pickle:ref_(t)
  local ref = self._tableToRef[t]
  if not ref then 
    if t == self then error("can't pickle the pickle class") end
    table.insert(self._refToTable, t)
    ref = table.getn(self._refToTable)
    self._tableToRef[t] = ref
  end
  return ref
end

----------------------------------------------
-- unpickle
----------------------------------------------

function unpickle(s)
  if type(s) ~= "string" then
    error("can't unpickle a "..type(s)..", only strings")
  end
  local gentables = loadstring("return "..s)
  local tables = gentables()
  
  for tnum = 1, table.getn(tables) do
    local t = tables[tnum]
    local tcopy = {}; for i, v in pairs(t) do tcopy[i] = v end
    for i, v in pairs(tcopy) do
      local ni, nv
      if type(i) == "table" then ni = tables[i[1]] else ni = i end
      if type(v) == "table" then nv = tables[v[1]] else nv = v end
      t[i] = nil
      t[ni] = nv
    end
  end
  return tables[1]
end

---========================================---
--- LIBRARY END
---========================================---

---========================================---
--- UTILITY
---
--- Various smaller objects for utility purposes
---========================================---

-- Util

function mergeInto(dest, source)
	if type(source) ~= "table" then
		return dest
	end
	for k, v in pairs(source) do
		if type(v) == "table" then 
			if type(v.copy) == "function" then
				dest[k] = v:copy()
			else
				-- No copying in this case
				dest[k] = v
			end
		else
			dest[k] = v
		end
	end
	return dest
end

-- A maximum 2 level copy
function copyValuesInto(dest, source, values)
	for _, v in ipairs(values) do
		if type(source[v]) == "table" then
			dest[v] = dest[v] or {}
			mergeInto(dest[v], source[v])
		else
			dest[v] = source[v]
		end
	end
end

function shallowCopyValues(dest, source, values)
	for _, v in ipairs(values) do
		dest[v] = source[v]
	end
end

function range(init, final, step)
	step = step or 1
	
	local range = {}
	for i = init, final, step do
		table.insert(range, i)
	end
	return range
end

function shallowCopy(orig)
    local orig_type = type(orig)
    local copy
    if orig_type == "table" then
        copy = {}
        for orig_key, orig_value in pairs(orig) do
            copy[orig_key] = orig_value
        end
    else -- number, string, boolean, etc
        copy = orig
    end
    return copy
end

function deepCopy(orig)
    local orig_type = type(orig)
    local copy
    if orig_type == 'table' then
        copy = {}
        for orig_key, orig_value in next, orig, nil do
            copy[deepCopy(orig_key)] = deepCopy(orig_value)
        end
        setmetatable(copy, deepCopy(getmetatable(orig)))
    else -- number, string, boolean, etc
        copy = orig
    end
    return copy
end

-- Find value
function tableIFind(tbl, toFind)
	for index, value in ipairs(tbl) do
		if value == toFind then
			return index
		end
	end
	return nil
end

-- Search for value satisfying condition
function tableISearch(tbl, condition)
	for index, value in ipairs(tbl) do
		if condition(value, index) then
			return index
		end
	end
	return nil
end

function makeUniqueIdGenerator()
	local id = 0
	return function()
		id = id + 1
		return id
	end
end

function writeTableToConsole(t)
	console.writeline(tostring(t))
	for k, v in pairs(t) do
		console.writeline(" * "..tostring(k).." = "..tostring(v))
	end
end

-- Class System
-- Note: 
Class = (function()
	-- Object functions
	local function implements(class, ...)
		if class._interfaces == nil then class._interfaces = {} end
		
		for _, interface in ipairs(arg) do
			table.insert(class._interfaces, inferface)
			for k, v in pairs(interface) do
				if (class[k] == nil) then
					class[k] = v
				end
			end
		end
		
		return class
	end
	
	local function is_a(class, baseclass)
		local base = class
		while base ~= nil do
			if base == baseclass then return true end
			base = base._base
		end
		return false
	end
	
	local function like_a(class, interface)
		local base = class
		while base ~= nil do
			local interfaces = base._interfaces
			if interfaces ~= nil then
				for _, implInterface in ipairs(interfaces) do
					if implInterface == interface then
						return true
					end
				end
			end
			base = base._base
		end
	end
	
	-- Syntactic sugar for object copies
	-- (without mentioning class)
	local function objectCopy(self)
		return self._base:copy(self)
	end
	
	local function new(class, o)
		o = o or {}
		setmetatable(o, class)
		o._base = class
		--o._base = class._base
		o._interfaces = class._interfaces
		o.copy = objectCopy
		return o
	end
	
	local function instantiate(class, ...)
		local o = class:new()
		o:init(...)
		return o
	end
	
	function Class(base, initialization)
		local class = {}
		
		--class._class = class
		class._base = base
		class.new = new
		class.is_a = is_a
		class.does_a = does_a
		class.implements = implements
		
		-- init should be overwritten for constructor arguments
		-- add a trivial one so base objects don't crash
		if base == nil then
			class.init = function () end
		else
			class.init = base.init
		end
		class._baseInit = class.init
		
		-- For objects
		class.__index = class
		
		-- For inheritance
		setmetatable(class, {
			__index = base,
			__call = instantiate
		})
		
		mergeInto(class, initialization)
		--writeTableToConsole(class)
		return class
	end
	
	return Class
end)()

Interface = (function()
	function Interface(base, initialization)
		local interface = {}
		
		mergeInto(interface, base)
		mergeInto(interface, initialization)
		
		return interface
	end
	
	return Interface
end)()
-- End Class System

-- Errors
function DEBUG(message)
	if type(message) == "table" then
		writeTableToConsole(message)
	else
		console.writeline(tostring(message))
	end
end

Error = {}

function Error:throw(message)
	DEBUG("ERROR: "..message)
	--console.writeline("ERROR: "..message)
end

-- Interface mechanism
function abstractFunction(self)
	Error:throw("Abstract method called in "..self)
end

function optionalFunction(self)

end

function implements(class, interface)
	for k, v in pairs(interface) do
		if (class[k] == nil) then
			class[k] = v
		else
			--DEBUG("Not overwriting "..k)
		end
	end
end

function implementsAll(class, ...)
	for v in values(arg) do
		implements(class, v)
	end
end

-- Events
Event = Class(nil)

function Event:init(source)
	self.source = source
	self.handlers = {}
end

function Event:add(func, context)
	table.insert(self.handlers, { f = func, c = context})
end

function Event:del(func)
	local handlers = self.handlers
	local index = tableISearch(handlers, function(handlerData)
		return handlerData.f == func
	end)
	if index ~= nil then
		table.remove(handlers, index)
	end
end

function Event:trigger(source)
	source = source or self.source
	for _, handlerData in ipairs(self.handlers) do
		handlerData.f(source, handlerData.c)
	end
end

---========================================---
--- UTILITY END
---========================================---

---========================================---
--- GAME
---
--- Functions to get key game specific values (sprites, etc.)
---========================================---

-- Interface Game
Game = Interface(nil, {
	defaultState = "0.state",
	buttonsNames = { },
	marioX = 0,
	marioY = 0,
	screenX = 0,
	screenY = 0,
	
	readValues = abstractFunction, -- ()
	getTile = abstractFunction, -- (x, y)
	getSprites = abstractFunction, -- ()
	getExtendedSprites = abstractFunction, -- ()
})
-- End Game

-- Class SMW
SMW = Class(nil, {
	rightEnd = 4816,
	defaultState = "DP1.state",
	buttonNames = {
                "A",
                "B",
                "X",
                "Y",
                "Up",
                "Down",
                "Left",
                "Right",
    }
}):implements(Game);

function SMW:readValues()
    self.marioX = memory.read_s16_le(0x94)
    self.marioY = memory.read_s16_le(0x96)
               
    local layer1x = memory.read_s16_le(0x1A);
    local layer1y = memory.read_s16_le(0x1C);

	marioX = self.marioX
	marioY = self.marioY
	
    self.screenX = marioX-layer1x
    self.screenY = marioY-layer1y
	
	screenX = self.screenX
	screenY = self.screenY
end

function SMW:getTile(dx, dy)
    x = math.floor((self.marioX+dx+8)/16)
    y = math.floor((self.marioY+dy)/16)
               
    return memory.readbyte(0x1C800 + math.floor(x/0x10)*0x1B0 + y*0x10 + x%0x10)
end

function SMW:getSprites()
	local sprites = {}
	for slot=0,11 do
			local status = memory.readbyte(0x14C8+slot)
			if status ~= 0 then
					spritex = memory.readbyte(0xE4+slot) + memory.readbyte(0x14E0+slot)*256
					spritey = memory.readbyte(0xD8+slot) + memory.readbyte(0x14D4+slot)*256
					sprites[#sprites+1] = {["x"]=spritex, ["y"]=spritey}
			end
	end            
   
	return sprites
end

function SMW:getExtendedSprites()
	local extended = {}
	for slot=0,11 do
			local number = memory.readbyte(0x170B+slot)
			if number ~= 0 then
					spritex = memory.readbyte(0x171F+slot) + memory.readbyte(0x1733+slot)*256
					spritey = memory.readbyte(0x1715+slot) + memory.readbyte(0x1729+slot)*256
					extended[#extended+1] = {["x"]=spritex, ["y"]=spritey}
			end
	end            
   
	return extended;
end

-- End SMW

-- Class SMB
SMB = Class(nil, {
	rightEnd = 3186,
	defaultState = "SMB1-1.state",
	buttonNames = {
                "A",
                "B",
                "X",
                "Y",
                "Up",
                "Down",
                "Left",
                "Right",
    }
}):implements(SMB, Game)

function SMB:readValues()
    self.marioX = memory.readbyte(0x6D) * 0x100 + memory.readbyte(0x86)
    self.marioY = memory.readbyte(0x03B8)+16
       
    self.screenX = memory.readbyte(0x03AD)
    self.screenY = memory.readbyte(0x03B8)
	
	marioX = self.marioX
	marioY = self.marioY
	screenX = self.screenX
	screenY = self.screenY
end

function SMB:getTile(dx, dy)
	local x = self.marioX + dx + 8
	local y = self.marioY + dy - 16
	local page = math.floor(x/256)%2

	local subx = math.floor((x%256)/16)
	local suby = math.floor((y - 32)/16)
	local addr = 0x500 + page*13*16+suby*16+subx
   
	if suby >= 13 or suby < 0 then
			return 0
	end
   
	if memory.readbyte(addr) ~= 0 then
			return 1
	else
			return 0
	end
end

function SMB:getSprites()
	local sprites = {}
	for slot=0,4 do
			local enemy = memory.readbyte(0xF+slot)
			if enemy ~= 0 then
					local ex = memory.readbyte(0x6E + slot)*0x100 + memory.readbyte(0x87+slot)
					local ey = memory.readbyte(0xCF + slot)+24
					sprites[#sprites+1] = {["x"]=ex,["y"]=ey}
			end
	end
   
	return sprites;
end

function SMB:getExtendedSprites()
	return {}
end

-- End SMB

currentGame = nil

if gameinfo.getromname() == "Super Mario World (USA)" then
	currentGame = SMW
elseif gameinfo.getromname() == "Super Mario Bros." then
	currentGame = SMB
end

Filename = currentGame.defaultState
ButtonNames = currentGame.buttonNames

-- Input/Output routines

-- Class JoypadUtil
JoypadUtil = Class(nil, {
	controller = {}
})

-- Must apply every frame
function JoypadUtil:apply()
	joypad.set(self.controller)
end

function JoypadUtil:set(buttons)
	self.controller = {}
	local controller = self.controller
	
	for _, button in ipairs(currentGame.buttonNames) do
		controller["P1 " .. button] = buttons[button]
	end
	
	-- Fix contradicting buttons
	if controller["P1 Left"] and controller["P1 Right"] then
			controller["P1 Left"] = false
			controller["P1 Right"] = false
	end
	if controller["P1 Up"] and controller["P1 Down"] then
			controller["P1 Up"] = false
			controller["P1 Down"] = false
	end
	
	self:apply()
end

function JoypadUtil:clear()
	self.controller = {}
	local controller = self.controller
	
	for b = 1,#ButtonNames do
			controller["P1 " .. ButtonNames[b]] = false
	end
	self:apply()
end
-- End JoypadUtil

---========================================---
--- GAME END
---========================================---

---========================================---
--- INPUT/OUTPUT
---
--- Handles values for the input and output nodes
---========================================---

-- Class IORegistry
IORegistry = Class(nil, {
	inputClasses = {},
	outputClasses = {}
})

function IORegistry.registerPrivate(loc, name, ioClass)
	loc[name] = ioClass
end

function IORegistry:registerInput(name, input)
	self.registerPrivate(self.inputClasses, name, input)
end

function IORegistry:registerOutput(name, input)
	self.registerPrivate(self.outputClasses, name, input)
end

function IORegistry.getPrivate(loc, map, spec)
	local ioSet = {}

	local names = spec:getNames()
	if #names == 1 then
		return loc[names[1]]
	end
	
	for _, name in ipairs(names) do
		ioSet[name] = loc[name]
	end
	
	return map(ioSet)
end

function IORegistry:getInput(inputSpec)
	return self.getPrivate(self.inputClasses, InputMap, inputSpec)
end

function IORegistry:getOutput(outputSpec)
	return self.getPrivate(self.outputClasses, OutputMap, outputSpec)
end
-- End IORegistry

-- Class IOConfig
IOConfig = Class(nil, {
	names = {},
	data = {},
})
-- Data format is list of:
--         { name = ?, id = ?, param = ? }
--		OR { name = ?, range = {?, ?}, param = ? }
--		OR { name = ?, param = ? }

function IOConfig:init(data, names)
	self.data = data
	
	self.names = names
	
	if names == nil then
		self.names = {}
		local foundNames = {}
		-- Automatically find names from data
		for _, datum in ipairs(data) do
			local name = datum.name
			if foundNames[name] == nil then
				table.insert(self.names, name)
			end
		end
	end
end

function IOConfig:copy(other)
	return IOConfig(other.data, other.names)
end

function IOConfig:getNames()
	return self.names
end

function IOConfig:iterator(ioObject)
	local dataIndex = 1
	local range = nil
	local rangeIndex = 1

	--writeTableToConsole(self.data)
	return function()
		if dataIndex > #self.data then
			return nil
		end
		
		local data = self.data[dataIndex]
		local name = data.name, id, handle
		
		if range == nil then
			if data.id ~= nil then
				dataIndex = dataIndex + 1
				id = data.id
			elseif data.range ~= nil then
				range = data.range
				rangeIndex = range[1]
			else
				range = {1, ioObject:sizeOf(name)}
				rangeIndex = range[1]
			end
		end
		
		-- If range, still need to do it
		if range ~= nil then
			id = rangeIndex
			rangeIndex = rangeIndex + 1
			if rangeIndex > range[2] then
				range = nil
				dataIndex = dataIndex + 1
			end
		end
		handle = ioObject:getHandle(name, id)
		return name, id, handle
	end
end

-- End IOConfig

-- Abstract Class IOMap
IOMap = Class(nil, {
	set = {},
	map = {},
	offsets = {},
	ioSizes = {},
	size = 0
})

function IOMap:init(set)
	self.set = {}
	
	local totalSize = 0
	for name, ioClass in pairs(set) do
		local size = ioClass.size
		ioSizes[name] = size
		offsets[name] = totalSize
		
		local data = { name = name, ioClass = ioClass, offset = totalSize}
		for i = totalSize+1, totalSize+size do
			map[i] = data
		end
		totalSize = totalSize + size
		
		table.insert(self.set, ioClass)
	end
	
	self.size = totalSize
end

function IOMap:getHandle(name, index)
	local offset = offsets[name]
	return offset+index
end

function IOMap:sizeOf(name)
	return self.ioSizes[name]
end
	
function IOMap:getAdjustedData(id)
	local data = map[id]
	local adjustedId =  id - data.offset
	return data.ioClass, adjustedId
end

-- End IOMap

---========================================---
--- INPUT/OUTPUT END
---========================================---

---========================================---
--- INPUT
---
--- Handles values for the input
---========================================---

-- Interface Input
Input = Interface(nil, {
	size = 0, -- Number of inputs
	prepare = abstractFunction, -- ()
	get = abstractFunction, -- (input id, [parameter])
	getHandle = function(self, name, index)
		return index
	end,
	sizeOf = function(self, name)
		return self.size
	end
})
-- End Input

-- Class InputMap
InputMap = Class(IOMap):implements(Input)

function InputMap:prepare()
	for _, input in ipairs(self.set) do
		input:prepare()
	end
end

function InputMap:get(id, param)
	local input, adjustedId =  self:getAdjustedData(id)
	return input:get(adjustedId, param)
end
-- End InputMap

-- Class TiledInput
BoxRadius = 6
InputSize = (BoxRadius*2+1)*(BoxRadius*2+1)

TiledInput = Class(nil, {
	sensed = {},
	size = InputSize+1,
}):implements(Input)

Inputs = TiledInput.size

function TiledInput:prepare()
	currentGame:readValues()
   
	local sprites = currentGame:getSprites()
	local extended = currentGame:getExtendedSprites()
   
	local id = 1
	local inputs = self.sensed
	
	for dy=-BoxRadius*16,BoxRadius*16,16 do
		for dx=-BoxRadius*16,BoxRadius*16,16 do
	
			inputs[id] = 0
		   
			tile = currentGame:getTile(dx, dy)
			if tile == 1 and marioY+dy < 0x1B0 then
					inputs[id] = 1
			end
		   
			for i = 1,#sprites do
					distx = math.abs(sprites[i]["x"] - (marioX+dx))
					disty = math.abs(sprites[i]["y"] - (marioY+dy))
					if distx <= 8 and disty <= 8 then
							inputs[id] = -1
					end
			end

			for i = 1,#extended do
					distx = math.abs(extended[i]["x"] - (marioX+dx))
					disty = math.abs(extended[i]["y"] - (marioY+dy))
					if distx < 8 and disty < 8 then
							inputs[id] = -1
					end
			end
			
			id = id + 1
		end
	end
	
	inputs[id] = 1
end

function TiledInput:get(id)
	return self.sensed[id]
end

IORegistry:registerInput("TiledInput", TiledInput)
-- End TiledInput

---========================================---
--- INPUT END
---========================================---

---========================================---
--- OUTPUT
---
--- Handles taking Neural Network result and sending it to controller
---========================================---

-- Interface Output
Output = Interface(nil, {
	size = 0, -- List of output ids and classification
	prepare = abstractFunction, -- ()
	set = abstractFunction, -- (output id, value, [parameter])
	send = abstractFunction, -- ()
	getHandle = function(self, name, index)
		return index
	end,
	sizeOf = function(self, name)
		return self.size
	end
})
-- End Output

-- Class OutputMap
OutputMap = Class(IOMap):implements(Output)

function OutputMap:prepare()
	for _, output in ipairs(self.set) do
		output:prepare()
	end
end

function OutputMap:set(id, value, param)
	local output, adjustedId =  self:getAdjustedData(id)
	return output:set(adjustedId, value, param)
end

function OutputMap:send()
	for _, output in ipairs(self.set) do
		output:send()
	end
end
-- End OutputMap

-- Class ButtonOutput

ButtonOutput = Class(nil, {
	size = #ButtonNames,
	buffer = {}
}):implements(Output)

Outputs = ButtonOutput.size

function ButtonOutput:prepare()
	for k, v in pairs(self.buffer) do
		self.buffer[k] = 0
	end
end

function ButtonOutput:set(id, value)
	self.buffer[id] = value
end

function ButtonOutput:send()
	local buttons = {}
	for o=1,Outputs do
		local button = ButtonNames[o]
		if self.buffer[o] ~= nil and self.buffer[o] > 0 then
			buttons[button] = true
		else
			buttons[button] = false
		end
	end
	--ButtonsToPress = buttons
	JoypadUtil:set(buttons)
end

IORegistry:registerOutput("ButtonOutput", ButtonOutput)
-- End ButtonOutput

---========================================---
--- OUTPUT END
---========================================---

---========================================---
--- RUNNABLE
---
--- Interfaces for runnable
---========================================---

-- Interface Runnable
Runnable = Interface(nil, {
	reset = abstractFunction, -- ()
	run = abstractFunction, -- ()
})
-- End Runnable

-- Interface Filter
Filter = Interface(Runnable, {
	init = abstractFunction, -- (configuration)
	setSource = function (self, source)
		self.source = source
	end,
	setSink = function (self, source)
		self.sink = source
	end,
})
-- End Filter

---========================================---
--- RUNNABLE END
---========================================---

---========================================---
--- NEURAL NETWORK
---
--- Data structure for NNs and evaluator
---========================================---

-- Class Neuron
-- Neurons in network
NEURON_INPUT = 0
NEURON_HIDDEN = 1
NEURON_OUTPUT = 2

Neuron = Class(nil, {
	id = 0,
	class = 0,
	ioIndex = 0, -- Only for input/output neurons
	param = nil, -- Only for some input/output neurons
	incoming = {},
	value = 0,
	
	NEURON_INPUT = 0,
	NEURON_HIDDEN = 1,
	NEURON_OUTPUT = 2,
})

function Neuron:init(class, index, param)
	self.incoming = {}
	
	self.class = class
	self.ioIndex = index
	self.param = param
end

function Neuron:makeInput(index, param)
	return Neuron(NEURON_INPUT, index, param)
end

function Neuron:makeOutput(index, param)
	return Neuron(NEURON_OUTPUT, index, param)
end

function Neuron:makeHidden()
	return Neuron(NEURON_HIDDEN)
end

function Neuron:addLinkInto(source, initWeight)
	table.insert(self.incoming, {source = source, weight = initWeight})
end

function Neuron:updateLink(source, weight)
	for _, link in pairs(self.incoming) do
		if link.source == source then
			link.weight = weight
			return
		end
	end
end
-- End Neuron

-- Class NeuralNetwork
-- Network layout and weights
NeuralNetwork = Class(nil, {
	--source = {},
	inputNeurons = {},
	outputNeurons = {},
	hiddenNeurons = {},
	maxId = 0,
})

function NeuralNetwork:init()
	self.inputNeurons = {}
	self.outputNeurons = {}
	self.hiddenNeurons = {}
	self.allNeurons = {}
	
	self.maxId = 1
end

function NeuralNetwork:copy(other)
	local o = self()
	
	copyValuesInto(o, other, {"inputNeurons", "outputNeurons", "hiddenNeurons", "maxId"})
	
	return o
end

function NeuralNetwork:find(neuronId)
	for _,neuron in ipairs(self.allNeurons) do
		if neuron.id == neuronId then
			return neuron
		end
	end
	return nil
end

-- Private
function NeuralNetwork:addNeuronPrivate(neuron, section)
	neuron.id = self.maxId
	self.maxId = self.maxId + 1
	table.insert(section, neuron)
	table.insert(self.allNeurons, neuron)
	return neuron
end

function NeuralNetwork:addInput(neuron)
	return self:addNeuronPrivate(neuron, self.inputNeurons)
end

function NeuralNetwork:addHidden(neuron)
	return self:addNeuronPrivate(neuron, self.hiddenNeurons)
end

function NeuralNetwork:addOutput(neuron)
	return self:addNeuronPrivate(neuron, self.outputNeurons)
end

-- Automatically use right Neuron class (input/output/hidden)
NeuralNetwork.addNeuron = (function()
	local addMap = {}
	addMap[NEURON_INPUT] = "addInput"
	addMap[NEURON_HIDDEN] = "addHidden"
	addMap[NEURON_OUTPUT] = "addOutput"
	return function(self, neuron)
		return self[addMap[neuron.class]](self, neuron)
	end
end)()

function NeuralNetwork:getLinks()
	local links = {}
	for _, neuron in ipairs(self.allNeurons) do
		for _, link in ipairs(neuron.incoming) do
			table.insert(links, {
				from = link.source,
				to = neuron.id,
				weight = link.weight })
		end
	end
	return links
end

-- End Neural Network

-- Interface NNExecutor
-- Network executor
NNExecutor = Interface(Filter, {
	source = {},
	sink = {},
	neuralNetwork = {},
})
-- End Interface

function sigmoid(x)
        return 2/(1+math.exp(-4.9*x))-1
end

-- Class SimpleNNExecutor
SimpleNNExecutor = Class(nil, {
	values = {}, -- Saved states of the NN Neurons
	transfer = sigmoid,
	
	source = {},
	sink = {},
}):implements(NNExecutor)

function SimpleNNExecutor:init(nn, source, sink)
	values = {}
	self.neuralNetwork = nn
	self.source = source
	self.sink = sink
end

function SimpleNNExecutor:copy(other)
	local o = self()
	
	shallowCopyValues(o, other, {"source", "sink", "neuralNetwork", "transfer"})
	copyValues(o, other, {"values"})
	return o
end

function SimpleNNExecutor:setNeuronValuePrivate(neuron, value)
	self.values[neuron.id] = value
	neuron.value = value
end

function SimpleNNExecutor:reset()
	for _, neuron in ipairs(self.neuralNetwork.allNeurons) do
		self:setNeuronValuePrivate(neuron, 0)
	end
end

function SimpleNNExecutor:processNeuron(neuron)
	local sum = 0
	for _, link in ipairs(neuron.incoming) do
		local sourceId = link.source
		sum = sum + link.weight * self.values[sourceId]
	end
   
	if #neuron.incoming > 0 then
		self:setNeuronValuePrivate(neuron, self.transfer(sum))
	end
end

function SimpleNNExecutor:run()
	local network = self.neuralNetwork
	
	for _, neuron in ipairs(network.inputNeurons) do
		self:setNeuronValuePrivate(neuron, self.source:get(neuron.ioIndex, neuron.param))
	end
   
	for _,neuron in ipairs(network.hiddenNeurons) do
		self:processNeuron(neuron)
	end
	
	self.sink:prepare()
   	for _,neuron in ipairs(network.outputNeurons) do
		self:processNeuron(neuron)
		self.sink:set(neuron.ioIndex, self.values[neuron.id], neuron.param)
	end
	self.sink:send()
end
-- End SimpleNNExecutor

---========================================---
--- NEURAL NETWORK END
---========================================---

---========================================---
--- GENOME
---
--- Parameters to construct some sort of 'evaluator'
--- And comparison functions
---========================================---

-- Abstract Class Genome
Genome = Class(nil, {
	genes = {},
	fitness = nil,
	id = 0,
	history = {},
	savedKeys = {"genes", "fitness", "id", "history"},
})

Genome.newId = makeUniqueIdGenerator()

function Genome:init()
	self.genes = {}
	self.id = self.newId()
	self.history = {}
	self.inputSpec = {}
	self.outputSpec = {}
end

-- Note: THE ID IS KEPT UNIQUE
function Genome:copy(other)
	local o = self()
	
	o.genes = deepCopy(other.genes)
	o.history = deepCopy(other.history)
	o.inputSpec = other.inputSpec:copy()
	o.outputSpec = other.outputSpec:copy()
	o.fitness = other.fitness
	o.id = other.newId()
	
	return o
end

function Genome:serialize()
	-- Don't save the computed configuration
	local savedVariables = {}
	for _, key in ipairs(self.savedKeys) do
		savedVariables[key] = self[key]
	end
	
	return pickle(savedVariables)
end

function Genome:deserialize(text)
	local o = unpickle(text)
	setmetatable(o, self)
	self.__index = self
	
	return o
end

function Genome:difference(other)
	if self == other then
		return 0.0
	end
	return 100.0
end

-- Override this
function Genome:makeGene()
	return {}
end

function Genome:copyGene(gene)
	return deepCopy(gene)
end

-- Override this
-- Private
function Genome:computeConfig(source, sink)
	return nil
end

function Genome:getConfig(source, sink)
	if self.config ~= nil then
		return self.config
	end
	self.config = self:computeConfig(source, sink)
	return self.config
end

-- Override this
function Genome:makeBaseOrganism(config)
	return Filter(config);
end

-- Returns the 'organism' (a filter) which can be run
function Genome:makeOrganism(ioCollection)
	local source = ioCollection:getInput(self.inputSpec)
	local sink = ioCollection:getOutput(self.outputSpec)
	
	return self:makeBaseOrganism(self:getConfig(source, sink),
		source, sink)
end
-- End Genome

-- Class NNGenome
NNGenome = Class(Genome, {
	savedKeys = {"genes", "fitness", "id", "history", "innovation"}
})

function NNGenome:init()
	self.inputSpec = IOConfig({ {name = "TiledInput"} })
	self.outputSpec = IOConfig({ {name = "ButtonOutput"} })
	self.genes = {}
	self.fitness = 0
	self.adjustedFitness = 0
	self.network = {}
	self.maxneuron = 0
	self.globalRank = 0
	self.mutationRates = {}
	self.mutationRates["connections"] = MutateConnectionsChance
	self.mutationRates["link"] = LinkMutationChance
	self.mutationRates["bias"] = BiasMutationChance
	self.mutationRates["node"] = NodeMutationChance
	self.mutationRates["enable"] = EnableMutationChance
	self.mutationRates["disable"] = DisableMutationChance
	self.mutationRates["step"] = StepSize
	
	self.geneNeuronMap = {}
end

function NNGenome:copy(other)
	local o = self._base.copy(self, other)
	
	copyValuesInto(o, other, {"adjustedFitness", "maxneuron"})
	o.mutationRates = deepCopy(other.mutationRates)
	return o
end

function NNGenome:makeGene()
	local gene = {}
	gene.into = 0
	gene.out = 0
	gene.weight = 0.0
	gene.enabled = true
	gene.innovation = 0
   
	return gene
end

-- The configuration is the network
function NNGenome:computeConfig(source, sink)
	local network = NeuralNetwork()
	
	local id
	self.geneNeuronMap = {}
	local idMap = self.geneNeuronMap
	
	local neuronCount = 1
	for name, id, handle in self.inputSpec:iterator(source) do
		id = network:addNeuron(Neuron:makeInput(handle)).id
		idMap[neuronCount] = id
		neuronCount = neuronCount + 1
	end

	
	for name, id, handle in self.outputSpec:iterator(sink) do
		id = network:addNeuron(Neuron:makeOutput(handle)).id
		idMap[neuronCount] = id
		neuronCount = neuronCount + 1
	end
	
	table.sort(self.genes, function (a,b)
			return (a.out < b.out)
	end)
	
	for i=1,#self.genes do
		local gene = self.genes[i]
		
		if gene.enabled then
			local id = idMap[gene.out]
			if id == nil then
					id = network:addNeuron(Neuron:makeHidden()).id
					idMap[gene.out] = id
			end
			
			local neuron = network:find(id)

			local id = idMap[gene.into]
			if id == nil then
				id = network:addNeuron(Neuron:makeHidden()).id
				idMap[gene.into] = id
			end
			
			neuron:addLinkInto(idMap[gene.into], gene.weight)
			--table.insert(neuron.incoming, gene)
		end
	end
		   
	return network
end

function NNGenome:makeBaseOrganism(config, source, sink)
	return SimpleNNExecutor(config, source, sink)
end

function NNGenome:difference(other)
	local dd = DeltaDisjoint*self:disjoint(self.genes, other.genes)
	local dw = DeltaWeights*self:weights(self.genes, other.genes)
	return dd+dw
end

function NNGenome:neuronIdForGene(geneEndPoint)
	return self.geneNeuronMap[geneEndPoint]
end

function NNGenome:randomNeuron(nonInput)
	local genes = self.genes
	
	local neurons = {}
	if not nonInput then
		for i=1,Inputs do
			neurons[i] = true
		end
	end
	for o=1,Outputs do
		neurons[Inputs+o] = true
	end
	for i=1,#genes do
		if (not nonInput) or genes[i].into > Inputs then
			neurons[genes[i].into] = true
		end
		if (not nonInput) or genes[i].out > Inputs then
			neurons[genes[i].out] = true
		end
	end

	local count = 0
	for _,_ in pairs(neurons) do
		count = count + 1
	end
	local n = math.random(1, count)
	
	for k,v in pairs(neurons) do
		n = n-1
		if n == 0 then
			return k
		end
	end
   
	return 0
end
 
function NNGenome:containsLink(link)
	local genes = self.genes
        for i=1,#genes do
                local gene = genes[i]
                if gene.into == link.into and gene.out == link.out then
                        return true
                end
        end
end
 
function NNGenome:disjoint(genes1, genes2)
	local i1 = {}
	for i = 1,#genes1 do
		local gene = genes1[i]
		i1[gene.innovation] = true
	end

	local i2 = {}
	for i = 1,#genes2 do
		local gene = genes2[i]
		i2[gene.innovation] = true
	end
   
	local disjointGenes = 0
	for i = 1,#genes1 do
		local gene = genes1[i]
		if not i2[gene.innovation] then
				disjointGenes = disjointGenes+1
		end
	end
   
	for i = 1,#genes2 do
		local gene = genes2[i]
		if not i1[gene.innovation] then
				disjointGenes = disjointGenes+1
		end
	end
   
	local n = math.max(#genes1, #genes2)
   
	return disjointGenes / n
end
 
function NNGenome:weights(genes1, genes2)
	local i2 = {}
	for i = 1,#genes2 do
		local gene = genes2[i]
		i2[gene.innovation] = gene
	end

	local sum = 0
	local coincident = 0
	for i = 1,#genes1 do
		local gene = genes1[i]
		if i2[gene.innovation] ~= nil then
			local gene2 = i2[gene.innovation]
			sum = sum + math.abs(gene.weight - gene2.weight)
			coincident = coincident + 1
		end
	end
   
	return sum / coincident
end

function NNGenome:makeStarterGenome()
	local genome = NNGenome()
	
	local breeder = NNBreeder():addParam({crossoverChance = CrossoverChance})
	genome.maxneuron = Inputs + Outputs
	
	breeder:mutate(genome)
   
	return genome
end

-- End NNGenome

---========================================---
--- GENOME END
---========================================---

---========================================---
--- BREEDER
---
--- Responsible for mixing different genomes
--- as well as for mutations
---========================================---

-- Interface Mutation
Mutation = Interface(nil, {
	param = {},
	-- Directly modifies passed in genes
	mutate = abstractFunction, -- (genes)
})
-- End Mutation

-- Abstract class SimpleMutation
SimpleMutation = Class(nil, {
	param = {},
	mutator = function(param, genome) return genome end,
}):implements(Mutation)

-- mutator is function (param, genome)
function SimpleMutation:init(mutator)
	self.param = shallowCopy(self._base.param)
	self.mutator = mutator
end

function SimpleMutation:addParam(initParam)
	mergeInto(self.param, initParam)
	return self
end
--[[
function SimpleMutation:make(mutator)
	local o = self:new()
	o.mutator = mutator
	
	return o
end]]--

function SimpleMutation:mutate(genome)
	self.mutator(self.param, genome)
end
-- End SimpleMutation

-- Abstract class MutationWithRate
MutationWithRate = Class(SimpleMutation)
MutationWithRate:addParam({ rate = 1.0 }) -- Default rate
--MutationWithRate = SimpleMutation:new():init({ rate = 1.0 })

function MutationWithRate:mutate(genome)
	local rate = self.param.rate
	while rate > 0 do
		if math.random() < rate then
			self.mutator(self.param, genome)
		end
		rate = rate - 1
	end
end
-- End MutationWithRate

-- Abstract class Breeder
Breeder = Class(nil, {
	param = {},
	mutations = {}
})

function Breeder:init()
	self.param = shallowCopy(self._base.param)
	self.mutations = shallowCopy(self._base.mutations)
--writeTableToConsole(self._base.mutations)
--writeTableToConsole(self.mutations)
end

function Breeder:addMutation(name, mutation, initParam)
	if initParam ~= nil then
		mergeInto(mutation.param, initParam)
	end
	self.mutations[name] = mutation
end

-- Parameters for mutations use [mutationName]:[key] format
function Breeder:addParam(param)
	local mutation, key
	for k, v in pairs(param) do
		if string.find(k, "^%w+:") then
			mutationId, key = string.match(k, "^(%w+):(.*)$")
		--writeTableToConsole(self.mutations)
			local mutation = self.mutations[mutationId]
			mutation.param[key] = v
		else
			self.param[k] = v
		end
	end
	return self
end

function Breeder:wantSex(genome)
	return false
end

function Breeder:asexualReproduction(genome)
	return genome:copy()
end

function Breeder:sexualReproduction(g1, g2)
	local child = g1:copy()
	mergeInto(child.genes, g2.genes)
	return child
end

function Breeder:choosePartner(genome, pool)
	return nil
end

function Breeder:reproduce(genome, pool)
	local child
	local partner
	if self:wantSex(genome) then
		partner = self:choosePartner(genome, pool)
	end
	if partner == nil then
		child = self:asexualReproduction(genome)
	else
		child = self:sexualReproduction(genome, partner)
	end
	return child
end

function Breeder:mutate(genome)
	for _, mutation in pairs(self.mutations) do
		mutation:mutate(genome)
	end
end

function Breeder:breed(genome, pool)
	local child = self:reproduce(genome, pool)
	self:mutate(child)
	return child
end
-- End Breeder

-- Class NNBreeder
NNBreeder = Class(Breeder, {
	mutations = {}
})

function pointMutation(param, genome)
	local step = param.step
	-- = genome.mutationRates["step"]
   
	for i=1,#genome.genes do
		local gene = genome.genes[i]
		if math.random() < PerturbChance then
			gene.weight = gene.weight + math.random() * step*2 - step
		else
			gene.weight = math.random()*4-2
		end
	end
end
NNBreeder:addMutation("connections",
	MutationWithRate(pointMutation))

function linkMutation(param, genome)
	local neuron1 = genome:randomNeuron(false) --randomNeuron(genome.genes, false)
	local neuron2 = genome:randomNeuron(true)  --randomNeuron(genome.genes, true)
	 
	local newLink = NNGenome:makeGene()
	if neuron1 <= Inputs and neuron2 <= Inputs then
			--Both input nodes
			return
	end
	if neuron2 <= Inputs then
			-- Swap output and input
			local temp = neuron1
			neuron1 = neuron2
			neuron2 = temp
	end

	newLink.into = neuron1
	newLink.out = neuron2
	if param.forceBias then
			newLink.into = Inputs
	end
   
	if genome:containsLink(newLink) then
			return
	end
	newLink.innovation = newInnovation()
	newLink.weight = math.random()*4-2
  
	table.insert(genome.genes, newLink)
end
NNBreeder:addMutation("link",
	MutationWithRate(linkMutation):addParam({forceBias = false}))
NNBreeder:addMutation("bias",
	MutationWithRate(linkMutation):addParam({forceBias = true}))
	
function nodeMutation(param, genome)
	if #genome.genes == 0 then
			return
	end

	genome.maxneuron = genome.maxneuron + 1

	local gene = genome.genes[math.random(1,#genome.genes)]
	if not gene.enabled then
			return
	end
	gene.enabled = false

	local gene1 = NNGenome:copyGene(gene)
	gene1.out = genome.maxneuron
	gene1.weight = 1.0
	gene1.innovation = newInnovation()
	gene1.enabled = true
	table.insert(genome.genes, gene1)
   
	local gene2 = NNGenome:copyGene(gene)
	gene2.into = genome.maxneuron
	gene2.innovation = newInnovation()
	gene2.enabled = true
	table.insert(genome.genes, gene2)
end
NNBreeder:addMutation("node",
	MutationWithRate(nodeMutation))

function enableDisableMutation(param, genome)
	local candidates = {}
	for _,gene in pairs(genome.genes) do
			if gene.enabled == not param.enable then
					table.insert(candidates, gene)
			end
	end
   
	if #candidates == 0 then
			return
	end
   
	local gene = candidates[math.random(1,#candidates)]
	gene.enabled = not gene.enabled
end
NNBreeder:addMutation("enable",
	MutationWithRate(enableDisableMutation):addParam({ enable = true }))
NNBreeder:addMutation("disable",
	MutationWithRate(enableDisableMutation):addParam({ enable = false }))

function NNBreeder:asexualReproduction(genome)
	return genome:copy() -- copyGenome(genome)
end

function NNBreeder:sexualReproduction(g1, g2)
	if g2.fitness > g1.fitness then
			tempg = g1
			g1 = g2
			g2 = tempg
	end

	local child = NNGenome()
   
	local innovations2 = {}
	for i=1,#g2.genes do
			local gene = g2.genes[i]
			innovations2[gene.innovation] = gene
	end
   
	for i=1,#g1.genes do
		local gene1 = g1.genes[i]
		local gene2 = innovations2[gene1.innovation]
		if gene2 ~= nil and math.random(2) == 1 and gene2.enabled then
			table.insert(child.genes, NNGenome:copyGene(gene2))
		else
			table.insert(child.genes, NNGenome:copyGene(gene1))
		end
	end
   
	child.maxneuron = math.max(g1.maxneuron,g2.maxneuron)
   
	for mutation,rate in pairs(g1.mutationRates) do
		child.mutationRates[mutation] = rate
	end
   
	return child
end

function NNBreeder:wantSex(genome)
	local crossoverChance = self.param["crossoverChance"]
	return math.random() < crossoverChance
end

function NNBreeder:choosePartner(genome, pool)
	return pool[math.random(1, #pool)]
end

function NNBreeder:mutate(genome)
	for mutation,rate in pairs(genome.mutationRates) do
		if math.random(1,2) == 1 then
			genome.mutationRates[mutation] = 0.95*rate
		else
			genome.mutationRates[mutation] = 1.05263*rate
		end
	end
	
	local param = {}
	local mutationTypes = {"connections", "link", "bias",
		"node", "enable", "disable"}
	for _, typeName in ipairs(mutationTypes) do
		param[typeName..":rate"] = genome.mutationRates[typeName]
	end
	param["connections:step"] = genome.mutationRates["step"]
	
	self:addParam(param)
	
	for _, mutation in pairs(self.mutations) do
		mutation:mutate(genome)
	end
end
-- End NNBreeder

---========================================---
--- BREEDER END
---========================================---

---========================================---
--- SPECIES
---========================================---

Species = Class(nil, {
	genomes = {}
})

function Species:init()
	self.topFitness = 0
	self.staleness = 0
	self.genomes = {}
	self.averageFitness = 0
end

function Species:calculateAverageFitness()
	local total = 0

	for _, genome in ipairs(self.genomes) do
		total = total + genome.globalRank
	end

	self.averageFitness = total / #self.genomes
end

---========================================---
--- SPECIES END
---========================================---

---========================================---
--- POPULATION
---
--- The collection of genomes
--- Responsible for organizing into reproductive groups (species)
--- And pruning + reproducing the population
---========================================---

Population = Class(nil, {
	maxSize = 50,
	species = {},
	genomes = {},
	maxFitness,
})

function Population:init(maxSize)	
	self.species = {}
	self.generation = 0
	self.maxFitness = 0
	
	self.maxSize = maxSize
end

function Population:addGenome(genome)
	local foundSpecies = false
	for s=1,#self.species do
		local species = self.species[s]
		if not foundSpecies and self:isInSameSpecies(genome, species.genomes[1]) then
			table.insert(species.genomes, genome)
			foundSpecies = true
		end
	end
   
	if not foundSpecies then
			local childSpecies = Species()
			table.insert(childSpecies.genomes, genome)
			table.insert(self.species, childSpecies)
	end
	
	table.insert(self.genomes, genome)
end

function Population:isInSameSpecies(genome1, genome2)
		local diff = genome1:difference(genome2)
        return diff < DeltaThreshold
end

function Population:rankGlobally()
        local global = {}
        for s = 1,#self.species do
                local species = self.species[s]
                for g = 1,#species.genomes do
                        table.insert(global, species.genomes[g])
                end
        end
        table.sort(global, function (a,b)
                return (a.fitness < b.fitness)
        end)
       
        for g=1,#global do
                global[g].globalRank = g
        end
end
 
function Population:calculateAverageFitness(species)
		species:calculateAverageFitness()
end
 
function Population:totalAverageFitness()
        local total = 0
        for s = 1,#self.species do
                local species = self.species[s]
                total = total + species.averageFitness
        end
 
        return total
end
 
function Population:cullSpecies(cutToOne)
        for s = 1,#self.species do
                local species = self.species[s]
               
                table.sort(species.genomes, function (a,b)
                        return (a.fitness > b.fitness)
                end)
               
                local remaining = math.ceil(#species.genomes/2)
                if cutToOne then
                        remaining = 1
                end
                while #species.genomes > remaining do
                        table.remove(species.genomes)
                end
        end
end

function Population:breedChild(species)
        local child = {}
		local original = species.genomes[math.random(1, #species.genomes)]

		local breeder = NNBreeder():addParam({crossoverChance = CrossoverChance})
		
        return breeder:breed(original, species.genomes)
end

function Population:removeStaleSpecies()
        local survived = {}
 
        for s = 1,#self.species do
                local species = self.species[s]
               
                table.sort(species.genomes, function (a,b)
                        return (a.fitness > b.fitness)
                end)
               
                if species.genomes[1].fitness > species.topFitness then
                        species.topFitness = species.genomes[1].fitness
                        species.staleness = 0
                else
                        species.staleness = species.staleness + 1
                end
                if species.staleness < StaleSpecies or species.topFitness >= self.maxFitness then
                        table.insert(survived, species)
                end
        end
 
        self.species = survived
end
 
function Population:removeWeakSpecies()
        local survived = {}
 
        local sum = self:totalAverageFitness()
        for s = 1,#self.species do
                local species = self.species[s]
                breed = math.floor(species.averageFitness / sum * self.maxSize)
                if breed >= 1 then
                        table.insert(survived, species)
                end
        end
 
        self.species = survived
end
 
function Population:newGeneration()
        self:cullSpecies(false) -- Cull the bottom half of each species
        self:rankGlobally()
        self:removeStaleSpecies()
        self:rankGlobally()
        for s = 1,#self.species do
                local species = self.species[s]
                self:calculateAverageFitness(species)
        end
        self:removeWeakSpecies()
        local sum = self:totalAverageFitness()
        local children = {}
        for s = 1,#self.species do
                local species = self.species[s]
                breed = math.floor(species.averageFitness / sum * self.maxSize) - 1
                for i=1,breed do
                        table.insert(children, self:breedChild(species))
                end
        end
        self:cullSpecies(true) -- Cull all but the top member of each species
        while #children + #self.species < self.maxSize do
                local species = self.species[math.random(1, #self.species)]
                table.insert(children, self:breedChild(species))
        end
        for c=1,#children do
                local child = children[c]
                self:addGenome(child)
        end
       
        self.generation = self.generation + 1
       
		local filename = UIConfig:getSaveLoadFile()
        self:writeFile("backup." .. self.generation .. "." .. filename)
end

function Population:writeFile(filename)
	local file = io.open(filename, "w")
	file:write(self.generation .. "\n")
	file:write(self.maxFitness .. "\n")
	file:write(#self.species .. "\n")
	for n,species in pairs(self.species) do
		file:write(species.topFitness .. "\n")
		file:write(species.staleness .. "\n")
		file:write(#species.genomes .. "\n")
		for m,genome in pairs(species.genomes) do
			file:write(genome.fitness .. "\n")
			file:write(genome.maxneuron .. "\n")
			for mutation,rate in pairs(genome.mutationRates) do
				file:write(mutation .. "\n")
				file:write(rate .. "\n")
			end
			file:write("done\n")
		   
			file:write(#genome.genes .. "\n")
			for l,gene in pairs(genome.genes) do
				file:write(gene.into .. " ")
				file:write(gene.out .. " ")
				file:write(gene.weight .. " ")
				file:write(gene.innovation .. " ")
				if(gene.enabled) then
						file:write("1\n")
				else
						file:write("0\n")
				end
			end
		end
	end
	file:close()
end
 
function Population:savePool()
	local filename = UIConfig:getSaveLoadFile()
	self:writeFile(filename)
end

function Population:loadFile(filename)
	local file = io.open(filename, "r")
	self = Population(PopulationMax)
	self.generation = file:read("*number")
	self.maxFitness = file:read("*number")
	UIConfig:setMaxFitness(self.maxFitness)
	local numSpecies = file:read("*number")
	for s=1,numSpecies do
		local species = Species()
		table.insert(self.species, species)
		species.topFitness = file:read("*number")
		species.staleness = file:read("*number")
		local numGenomes = file:read("*number")
		for g=1,numGenomes do
			local genome = NNGenome()
			table.insert(species.genomes, genome)
			genome.fitness = file:read("*number")
			genome.maxneuron = file:read("*number")
			local line = file:read("*line")
			while line ~= "done" do
				genome.mutationRates[line] = file:read("*number")
				line = file:read("*line")
			end
			local numGenes = file:read("*number")
			for n=1,numGenes do
				local gene = NNGenome:makeGene()
				table.insert(genome.genes, gene)
				local enabled
				gene.into, gene.out, gene.weight, gene.innovation, enabled = file:read("*number", "*number", "*number", "*number", "*number")
				if enabled == 0 then
					gene.enabled = false
				else
					gene.enabled = true
				end
			   
			end
		end
	end
	file:close()
   
	popSim:init(self, nil)
	while popSim:fitnessAlreadyMeasured() do
			popSim:nextGenome()
	end
end

function Population:loadPool()
	local filename = UIConfig:getSaveLoadFile()
	self:loadFile(filename)
end

function Population:makeStarter()
	local pop = Population(PopulationMax)
	
	for i=1,PopulationMax do
		local basic = NNGenome:makeStarterGenome()
		pop:addGenome(basic)
	end

	return pop
end

function Population:countMeasured()
	local measured = 0
	local total = 0
	for _,species in pairs(self.species) do
		for _,genome in pairs(species.genomes) do
			total = total + 1
			if genome.fitness ~= 0 then
				measured = measured + 1
			end
		end
	end
	return measured, total
end

---========================================---
--- POPULATION END
---========================================---

---========================================---
--- JUDGE
---
--- Computes the fitness for the organism
---========================================---

-- Interface Judge
Judge = Interface(nil, {
	setOrganism = abstractFunction, -- (organism)
	setup = abstractFunction, -- ()
	step = abstractFunction, -- ()
	measure = abstractFunction, -- ()
	shouldEnd = abstractFunction, -- ()
	finalMeasurement = abstractFunction, -- ()
})
-- End Judge

-- Abstract Class BasicJudge
BasicJudge = Class(nil, {
	organism = nil,
	frame = nil
}):implements(Judge)

function BasicJudge:setOrganism(organism)
	self.organism = organism
end

function BasicJudge:setup()
	self.frame = 0
end

function BasicJudge:step()
	self.frame = self.frame + 1
end

function BasicJudge:finalMeasurement()
	return self:measure()
end
-- End BasicJudge

-- Rightmost Judge
RightmostJudge = Class(BasicJudge)

function RightmostJudge:setup()
	self._base.setup(self)
	self.rightmost = 0
	self.timeout = 0
end

function RightmostJudge:step()
	self._base.step(self)
	
	local frame = self.frame
	
	currentGame:readValues()
	if marioX > self.rightmost then
			self.rightmost = marioX
			self.timeout = (4*TimeoutConstant+frame)/3
	end

	self.timeout = self.timeout - 1
end

-- Off by one because of an extra step?
function RightmostJudge:measure()
	local frame = self.frame - 1
	local fitness = math.floor(self.rightmost -  frame / 2 - self.timeout/2)
	return fitness
end

function RightmostJudge:finalMeasurement()
	local frame = self.frame - 1
	local fitness = self.rightmost - frame / 2
	
	if self.rightmost > currentGame.rightEnd then
		fitness = fitness + 1000
	end
	if fitness == 0 then
			fitness = -1
	end
	return fitness
end

function RightmostJudge:shouldEnd()
	return self.timeout <= 0
end

---========================================---
--- JUDGE END
---========================================---

---========================================---
--- SIMULATION
---
--- Simulation of the run of a genome
--- and the larger simulation of a population
---========================================---

-- Interface Simulation
Simulation = Interface(nil, {
	setup = abstractFunction, -- ([parameters])
	step = abstractFunction, -- ()
	isDone = abstractFunction, -- ()
})
-- End Simulation

-- Abstract Class BasicSimulation
BasicSimulation = Class(nil, {
	frame = 0,
	done = false,
	--onComplete = Event(), -- functions to call when the simulation is complete
	
	evaluate = abstractFunction,
}):implements(Simulation)

function BasicSimulation:init()
	self.done = false
	self.onComplete = Event(self)
	self.frame = 0
end

function BasicSimulation:step()
	self.frame = self.frame + 1
	self:evaluate()
	if self:isDone() then
		self.done = true
		self:finish()
		self.onComplete:trigger()
	end
end

function BasicSimulation:finish()

end

-- End BasicSimulation

-- Class GenomeSimulation
GenomeSimulation = Class(BasicSimulation)

function GenomeSimulation:init(genome, evaluator)
	self:_baseInit()
	
	self.done = false
	self.genome = genome
	self.evaluator = evalutor
	self.fitness = 0
	self.maxFitness = 0
	
	self.evaluator = RightmostJudge
end

function GenomeSimulation:setup(param)
	savestate.load(Filename);
	
	self.rightmost = 0
	self.timeout = TimeoutConstant
	JoypadUtil:clear()
	
	local genome = self.genome
	
	local organism = genome:makeOrganism(IORegistry)
	organism:reset()
	
	self.evaluator:setOrganism(organism)
	self.evaluator:setup()
	
	genome.organism = organism
	
	self:runEvaluation()
end

function GenomeSimulation:isDone()
	return self.evaluator:shouldEnd()
end

function GenomeSimulation:runEvaluation()
	local genome = self.genome
	
	--inputs = getInputs()
	TiledInput:prepare()
	
	genome.organism:run()
end

function GenomeSimulation:evaluate()
	if self.frame%5 == 0 then
		self:runEvaluation()
	end
	self.evaluator:step()
end

function GenomeSimulation:finish()
	local genome = self.genome

	local fitness = self.evaluator:finalMeasurement()
	genome.fitness = fitness
	self.fitness = fitness
	
	-- TODO: Don't reference popSim
	local pop = popSim.population
	if fitness > pop.maxFitness then
			pop.maxFitness = fitness
			UIConfig:setMaxFitness(pop.maxFitness)
			
			local filename = UIConfig:getSaveLoadFile()
			pop:writeFile("backup." .. pop.generation .. "." .. filename)
	end
end

function GenomeSimulation:getCurrentFitness()
	return self.evaluator:measure()
end
-- End GenomeSimulation

-- PopulationSimulation
PopulationSimulation = Class(BasicSimulation)

function PopulationSimulation:init(population, genomeSimFactory)
	self:_baseInit()

	self.population = population
	self.genomeSimFactory = genomeSimFactory
	self.genomeSimulation = nil
	self.genomeSimDone = false
	
	self.currentSpecies = 1
	self.currentGenome = 1
	
	return self
end

function PopulationSimulation:setup(param)
	--initializeRun()
end

function PopulationSimulation:isDone()
	return self.currentSpecies > #self.population.species
end

function PopulationSimulation:genomeSimDoneHandler(sim)
	self.genomeSimDone = true
	
	console.writeline("Gen " .. self.population.generation .. " species " .. self.currentSpecies .. " genome " .. self.currentGenome .. " fitness: " .. sim.fitness)
	
	self.currentSpecies = 1
	self.currentGenome = 1
end

function PopulationSimulation:evaluate()
	if self.genomeSimulation == nil then
		self:initializeGenomeSim()
	end
	
	if self.genomeSimulation then
		self.genomeSimulation:step()
	end
	
	if self.genomeSimDone then
		self.genomeSimulation = nil
		while self:fitnessAlreadyMeasured() do
			self:nextGenome()
		end
		--initializeRun()
	end
end


function PopulationSimulation:reset()
	popSim.currentSpecies = 1
	popSim.currentGenome = 1
end

function PopulationSimulation:finish()
	self.population:newGeneration()
	
	popSim.currentSpecies = 1
	popSim.currentGenome = 1
end

function PopulationSimulation:nextGenome()
	local population = self.population
	self.currentGenome = self.currentGenome + 1
	if self.currentGenome > #population.species[self.currentSpecies].genomes then
		self.currentGenome = 1
		self.currentSpecies = self.currentSpecies+1
	end
end

function PopulationSimulation:fitnessAlreadyMeasured()
	local species = self.population.species[self.currentSpecies]
	
	if species == nil then return false
	end
	
	local genome = species.genomes[self.currentGenome]
  --writeTableToConsole(genome)
	return genome.fitness ~= 0
end

function PopulationSimulation:initializeGenomeSim()
	savestate.load("DP1.state");
	--savestate.load(Filename);
	--rightmost = 0
	--timeout = TimeoutConstant
	JoypadUtil:clear()
	
	local species = self.population.species[self.currentSpecies]
	local genome = species.genomes[self.currentGenome]
	
	local genomeSimulation = GenomeSimulation(genome, nil, nil)
	genomeSimulation:setup()
	genomeSimulation:step()
	self.genomeSimulation = genomeSimulation
	--self.genomeSimulation = genomeSimFactory(genome)
	
	self.genomeSimDone = false
	self.genomeSimulation.onComplete:add(function (sim)
		self:genomeSimDoneHandler(sim)
	end)
end

function PopulationSimulation:activeGenome()
	local species = self.population.species[self.currentSpecies]
	local genome = species.genomes[self.currentGenome]
	
	return genome
end

function PopulationSimulation:getCurrentSpecies()
	return self.currentSpecies
end

function PopulationSimulation:getCurrentGenome()
	return self.currentGenome
end

function PopulationSimulation:getFrame()
	local frame = 1
	if self.genomeSimulation then
		frame = self.genomeSimulation.frame
	end
	return frame-2
end

function PopulationSimulation:getCurrentFitness()
	local genomeSim = self.genomeSimulation
	if genomeSim == nil then
		return 0
	end
	
	return genomeSim:getCurrentFitness()
	
end

function PopulationSimulation:playTop()
	local maxfitness = 0
	local maxs, maxg
	for s,species in pairs(self.population.species) do
		for g,genome in pairs(species.genomes) do
			if genome.fitness > maxfitness then
				maxfitness = genome.fitness
				maxs = s
				maxg = g
			end
		end
	end
   
	self.currentSpecies = maxs
	self.currentGenome = maxg
	self.maxFitness = maxfitness
	self.genomeSimulation = nil
	
	UIConfig:setMaxFitness(self.maxFitness)
	return
end
-- End PopulationSimulation

-- Final Class SimulationManager
SimulationManager = Class(nil, {
	simulationsInProgress = {},
	
})

SimulationManager.completionHandler = function(simulation)
	SimulationManager:RemoveSimulation(simulation)
end

function SimulationManager:AddSimulation(simulation, parameters, persist)
	if persist == nil then persist = false end
	
	table.insert(self.simulationsInProgress, {
		simulation = simulation,
		param = parameters,
		running = false,
		persist = persist
	})
	simulation.onComplete:add(self.completionHandler)
end

function SimulationManager:EndSimulation(simulation)
	local index = tableISearch(self.simulationsInProgress, function (v)
		if v.simulation == simulation then
			return true
		end
	end)
	if index ~= nil then
		local data = self.simulationsInProgress[index]
		if not data.persist then
			table.remove(self.simulationsInProgress, index)
		else
			data.running = false -- Forcing re-initialization
		end
	end
end

function SimulationManager:step()
	for _, simData in ipairs(self.simulationsInProgress) do
		local sim = simData.simulation
		if not simData.running then
			sim:setup(simData.parameters)
			simData.running = true
		end
		sim:step()
	end
end
-- End SimulationManager

---========================================---
--- SIMULATION END
---========================================---

---========================================---
--- UI
---
--- Graphical displays for the simulation
---========================================---

-- Interface UI
UI = Interface(nil, {
	layout = abstractMethod, --(x, y)
	draw = abstractMethod, -- ()
})
-- End UI

-- Abstract Class BasicUI
BasicUI = Class(nil, {
	x = 0,
	y = 0
}):implements(UI)

function BasicUI:init(x, y)
	x = x or 0
	y = y or 0
	
	self.x = x
	self.y = y
end

function BasicUI:layout(x, y)
	self.x = x
	self.y = y
end
-- End BasicUI

-- Class NNUI
NNUI = Class(BasicUI, {
--[[
	inputTiles = { x = 50, y = 70, xgap = 5, ygap = 5,
		colorBorder = 0xFF000000, colorBackground = 0x80808080},
	biasCell = { x = 80, y = 110 },
	outputs = { x = 220, y = 30, ygap = 8 },
	outputNames = { x = 223, y = 24, ygap = 8,
		colorOn = 0xFF0000FF, colorOff = 0xFF000000,
		fontSize = 9},
	hidden = { x = 140, y = 140 },
	neuron = { opacity = 0xFF000000, opacityInactive = 0x50000000, radius = 2 },
	links = { opacity = 0xA0000000, opacityInactive = 0x20000000,
		colorFunc = function(weight)
			local brightness = 0x80-math.floor(math.abs(sigmoid(weight))*0x80)
			if weight > 0 then
				return 0x8000 + 0x10000*brightness
			else
				return 0x800000 + 0x100*brightness
			end
		end },
	box = { colorBorder = 0x00000000, colorBackground = 0x80FF0000 }]]--
})

function NNUI:init(...)
	self._baseInit(...)
	
	self.cells = {}
	self.inputCells = {}
	self.outputCells = {}
	self.internalCells = {}
	
	self.needCellGeneration = true
	self.network = nil
end

function NNUI:drawCell(cell)
	local x, y, value = cell.x, cell.y, cell.value
	
	local color = math.floor((value+1)/2*256)
	if color > 255 then color = 255 end
	if color < 0 then color = 0 end
	local opacity = 0xFF000000
	if value == 0 then
		opacity = 0x50000000
	end
	color = opacity + color*0x10000 + color*0x100 + color
	gui.drawBox(x-2,y-2,x+2,y+2,opacity,color)
end

function NNUI:drawInput(x, y)
	local network = self.network
	
	if self.needCellGeneration then
		self.inputCells = {}
	
		local i = 1
		for row = 0, 2*BoxRadius do
			for col = 0, 2*BoxRadius do
				cell = {
					x = 20 + 5*col,
					y = 40 + 5*row,
					neuron = network.inputNeurons[i],
					value = network.inputNeurons[i].value,
					fixed = true
				}
				table.insert(self.inputCells, cell)
				table.insert(self.cells, cell)
				i = i + 1
			end
		end
		
		local biasCell = {
			x = 80,
			y = 110,
			neuron = network.inputNeurons[#network.inputNeurons],
			value = network.inputNeurons[#network.inputNeurons].value,
			fixed = true
		}
		table.insert(self.inputCells, biasCell)
		table.insert(self.cells, biasCell)
	end
	
	gui.drawBox(50-BoxRadius*5-3,70-BoxRadius*5-3,50+BoxRadius*5+2,70+BoxRadius*5+2,0xFF000000, 0x80808080)
	
	for _, cell in ipairs(self.inputCells) do
		cell.value = cell.neuron.value
		if cell.value ~= 0 then
			self:drawCell(cell)
		end
	end
end

function NNUI:drawOutput(x, y)
	local network = self.network
	
	if self.needCellGeneration then
		self.outputCells = {}
		
		local i = Inputs
		for o = 1,Outputs do
			cell = {
				x = 220,
				y = 30 + 8 * o,
				neuron = network.outputNeurons[o],
				value = network.outputNeurons[o].value,
				fixed = true,
				ioId = o
			}
			table.insert(self.outputCells, cell)
			table.insert(self.cells, cell)
			
			local color
			if cell.value > 0 then
				color = 0xFF0000FF
			else
				color = 0xFF000000
			end
			gui.drawText(223, 24+8*o, ButtonNames[o], color, 9)
		end
	end
	
	for _, cell in ipairs(self.outputCells) do
		local color
		cell.value = cell.neuron.value
		
		if cell.value > 0 then
			color = 0xFF0000FF
		else
			color = 0xFF000000
		end
		gui.drawText(223, 24+8*cell.ioId, ButtonNames[cell.ioId], color, 9)
		
		self:drawCell(cell)
	end
end

function NNUI:organizeNetwork(count)
	local cells = self.cells
	for n=1,count do
		for _,link in ipairs(self.links) do
			local sourceId = link.from
			local sinkId = link.to
			local weight = link.weight
			
			local c1 = cells[sourceId]
			local c2 = cells[sinkId]
			
			if not c1.fixed then
				c1.x = 0.75*c1.x + 0.25*c2.x
				if c1.x >= c2.x then
						c1.x = c1.x - 40
				end
				if c1.x < 90 then
						c1.x = 90
				end
			   
				if c1.x > 220 then
						c1.x = 220
				end
				c1.y = 0.75*c1.y + 0.25*c2.y
			end
			
			if not c2.fixed then
				c2.x = 0.25*c1.x + 0.75*c2.x
				if c1.x >= c2.x then
						c2.x = c2.x + 40
				end
				if c2.x < 90 then
						c2.x = 90
				end
				if c2.x > 220 then
						c2.x = 220
				end
				c2.y = 0.25*c1.y + 0.75*c2.y
			end
		end
	end
end

function NNUI:drawHidden()
	local network = self.network
	
	if self.needCellGeneration then
		self.hiddenCells = {}
		for index,neuron in pairs(network.hiddenNeurons) do
			cell = {
				x = 140,
				y = 40,
				neuron = neuron,
				value = neuron.value,
				fixed = false
			}
			table.insert(self.hiddenCells, cell)
			table.insert(self.cells, cell)
		end
		
		self:organizeNetwork(4)
	end
	
	for _, cell in ipairs(self.hiddenCells) do
		cell.value = cell.neuron.value
		self:drawCell(cell)
	end
end

function NNUI:drawLinks()
	local cells = self.cells
	for _, link in ipairs(self.links) do
		local sourceId = link.from
		local sinkId = link.to
		local weight = link.weight
		
		local c1 = cells[sourceId]
		local c2 = cells[sinkId]
		
		local opacity = 0xA0000000
		if c1.value == 0 then
				opacity = 0x20000000
		end
	   
		local color = 0x80-math.floor(math.abs(sigmoid(weight))*0x80)
		if weight > 0 then
				color = opacity + 0x8000 + 0x10000*color
		else
				color = opacity + 0x800000 + 0x100*color
		end
		gui.drawLine(c1.x+1, c1.y, c2.x-3, c2.y, color)
	end
end

function NNUI:drawMario()
	gui.drawBox(49,71,51,78,0x00000000,0x80FF0000)
end

function NNUI:draw()
	local genome = popSim:activeGenome()
	if genome.organism == nil then return end
	local network = genome.organism.neuralNetwork
	
	if network ~= self.network then
		self.needCellGeneration = true
		self.cells = {}
		self.network = network
		self.links = network:getLinks()
	else
		self.needCellGeneration = false
	end
	
	self:drawInput()
	self:drawOutput()
	self:drawHidden()
	self:drawLinks()
	self:drawMario()
end
-- End NNUI

-- Class MutationRatesUI
MutationRatesUI = Class(BasicUI)

function MutationRatesUI:draw()
	local genome = popSim:activeGenome()
	local pos = 100
DEBUG("mutat")
	local color = 0xFF000000
	color = 0xFFFFFFFF
	for mutation,rate in pairs(genome.mutationRates) do
		gui.drawText(100, pos, mutation .. ": " .. rate, color, 10)
		pos = pos + 8
	end
end
-- End MutationRatesUI

-- Class BannerUI
BannerUI = Class(BasicUI)

function BannerUI:draw()
	local backgroundColor = 0xD0FFFFFF
	gui.drawBox(0, 0, 300, 26, backgroundColor, backgroundColor)
		
	local pop = popSim.population
	local timeoutBonus = popSim:getFrame() / 4
	local measured, total = pop:countMeasured()

	local species = popSim:getCurrentSpecies()
	local genome = popSim:getCurrentGenome()
	local frame = popSim:getFrame()
	
	gui.drawText(0, 0, "Gen " .. pop.generation .. " species " .. species .. " genome " .. genome .. " (" .. math.floor(measured/total*100) .. "%)", 0xFF000000, 11)
	gui.drawText(0, 12, "Fitness: " .. popSim:getCurrentFitness(), 0xFF000000, 11)
	gui.drawText(100, 12, "Max Fitness: " .. math.floor(pop.maxFitness), 0xFF000000, 11)
end
-- End BannerUI

-- Class UIConfig
UIConfig = Class(nil,{
	form = nil
})

function UIConfig:show()
	local form = forms.newform(200, 260, "Fitness")
	self.form = form
	local pop = popSim.population
	
	self.maxFitnessLabel = forms.label(form, "Max Fitness: " .. math.floor(pop.maxFitness), 5, 8)
	self.showNetworkCheck = forms.checkbox(form, "Show Map", 5, 30)
	self.showMutationRatesCheck = forms.checkbox(form, "Show M-Rates", 5, 52)
	self.restartButton = forms.button(form, "Restart", function()
			popSim = PopulationSimulation(Population:makeStarter(), nil)
		end, 5, 77)
		
	self.saveButton = forms.button(form, "Save", function()
			popSim.population:savePool()
		end, 5, 102)
	self.loadButton = forms.button(form, "Load", function()
			popSim.population:loadPool()
		end, 80, 102)
	
	self.saveLoadFileBox = forms.textbox(form, Filename .. ".pool", 170, 25, nil, 5, 148)
	self.saveLoadLabel = forms.label(form, "Save/Load:", 5, 129)
	self.playTopButton = forms.button(form, "Play Top", function()
			popSim:playTop()
		end, 5, 170)
	self.hideBannerCheck = forms.checkbox(form, "Hide Banner", 5, 190)
end

function UIConfig:dispose()
	if self.form then
		forms.destroy(self.form)
	end
end

function UIConfig:showMutationRates()
	local form = self.form
	return form and forms.ischecked(self.showMutationRatesCheck)
end

function UIConfig:showNetwork()
	local form = self.form
	return form and forms.ischecked(self.showNetworkCheck)
end

function UIConfig:hideBanner()
	local form = self.form
	return form and forms.ischecked(self.hideBannerCheck)
end

function UIConfig:getSaveLoadFile()
	local form = self.form
	if form ~= nil then
		return forms.gettext(self.saveLoadFileBox)
	else
		return "DP1.state.pool"
	end
end

function UIConfig:setMaxFitness(fitness)
	if self.form == nil then return end
	forms.settext(self.maxFitnessLabel, "Max Fitness: " .. math.floor(fitness))
end
-- End UIConfig

---========================================---
--- UI END
---========================================---

PopulationMax = 10
DeltaDisjoint = 2.0
DeltaWeights = 0.4
DeltaThreshold = 1.0
 
StaleSpecies = 15
 
MutateConnectionsChance = 0.25
PerturbChance = 0.90
CrossoverChance = 0.75
LinkMutationChance = 2.0
NodeMutationChance = 0.50
BiasMutationChance = 0.40
StepSize = 0.1
DisableMutationChance = 0.4
EnableMutationChance = 0.2
 
TimeoutConstant = 20
 
MaxNodes = 1000000

newInnovation = makeUniqueIdGenerator()

popSim = nil
popSim = PopulationSimulation(Population:makeStarter(), nil)

UIConfig:show()

event.onexit(function ()
	UIConfig:dispose()
end)

while true do
	local genome = popSim:activeGenome()

	if UIConfig:showNetwork() then
		NNUI:draw()
	end
   if UIConfig:showMutationRates() then
		MutationRatesUI:draw()
	end
	
	popSim:step()
	JoypadUtil:apply()

	if not UIConfig:hideBanner()then
		BannerUI:draw()
	end

	emu.frameadvance();
end
