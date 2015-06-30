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
		if type(v) ~= "function" then -- Added condition - sockfolder
			s = string.format("%s[%s]=%s,\n", s, self:value_(i), self:value_(v))
		end
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
  else error("pickle a "..type(v).." is not supported")
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


----------------------------------------------
-- TabluePersistence.lua
----------------------------------------------

do
	local write, writeIndent, writers, refCount;

	persistence =
	{
		store = function (path, ...)
			local file, e = io.open(path, "w");
			if not file then
				return error(e);
			end
			local n = select("#", ...);
			-- Count references
			local objRefCount = {}; -- Stores reference that will be exported
			for i = 1, n do
				refCount(objRefCount, (select(i,...)));
			end;
			-- Export Objects with more than one ref and assign name
			-- First, create empty tables for each
			local objRefNames = {};
			local objRefIdx = 0;
			file:write("-- Persistent Data\n");
			file:write("local multiRefObjects = {\n");
			for obj, count in pairs(objRefCount) do
				if count > 1 then
					objRefIdx = objRefIdx + 1;
					objRefNames[obj] = objRefIdx;
					file:write("{};"); -- table objRefIdx
				end;
			end;
			file:write("\n} -- multiRefObjects\n");
			-- Then fill them (this requires all empty multiRefObjects to exist)
			for obj, idx in pairs(objRefNames) do
				for k, v in pairs(obj) do
					file:write("multiRefObjects["..idx.."][");
					write(file, k, 0, objRefNames);
					file:write("] = ");
					write(file, v, 0, objRefNames);
					file:write(";\n");
				end;
			end;
			-- Create the remaining objects
			for i = 1, n do
				file:write("local ".."obj"..i.." = ");
				write(file, (select(i,...)), 0, objRefNames);
				file:write("\n");
			end
			-- Return them
			if n > 0 then
				file:write("return obj1");
				for i = 2, n do
					file:write(" ,obj"..i);
				end;
				file:write("\n");
			else
				file:write("return\n");
			end;
			if type(path) == "string" then
				file:close();
			end;
		end;

		load = function (path)
			if type(path) == "string" then
				local f, e = loadfile(path);
			else
				local f, e = path:read('*a')
			end
			if f then
				return f();
			else
				return nil, e;
			end;
		end;
	}

	-- Private methods

	-- write thing (dispatcher)
	write = function (file, item, level, objRefNames)
		writers[type(item)](file, item, level, objRefNames);
	end;

	-- write indent
	writeIndent = function (file, level)
		for i = 1, level do
			file:write("\t");
		end;
	end;

	-- recursively count references
	refCount = function (objRefCount, item)
		-- only count reference types (tables)
		if type(item) == "table" then
			-- Increase ref count
			if objRefCount[item] then
				objRefCount[item] = objRefCount[item] + 1;
			else
				objRefCount[item] = 1;
				-- If first encounter, traverse
				for k, v in pairs(item) do
					refCount(objRefCount, k);
					refCount(objRefCount, v);
				end;
			end;
		end;
	end;

	-- Format items for the purpose of restoring
	writers = {
		["nil"] = function (file, item)
				file:write("nil");
			end;
		["number"] = function (file, item)
				file:write(tostring(item));
			end;
		["string"] = function (file, item)
				file:write(string.format("%q", item));
			end;
		["boolean"] = function (file, item)
				if item then
					file:write("true");
				else
					file:write("false");
				end
			end;
		["table"] = function (file, item, level, objRefNames)
				local refIdx = objRefNames[item];
				if refIdx then
					-- Table with multiple references
					file:write("multiRefObjects["..refIdx.."]");
				else
					-- Single use table
					file:write("{\n");
					for k, v in pairs(item) do
						writeIndent(file, level+1);
						file:write("[");
						write(file, k, level+1, objRefNames);
						file:write("] = ");
						write(file, v, level+1, objRefNames);
						file:write(";\n");
					end
					writeIndent(file, level);
					file:write("}");
				end;
			end;
		["function"] = function (file, item)
				-- Does only work for "normal" functions, not those
				-- with upvalues or c functions
				local dInfo = debug.getinfo(item, "uS");
				if dInfo.nups > 0 then
					file:write("nil --[[functions with upvalue not supported]]");
				elseif dInfo.what ~= "Lua" then
					file:write("nil --[[non-lua function not supported]]");
				else
					local r, s = pcall(string.dump,item);
					if r then
						file:write(string.format("loadstring(%q)", s));
					else
						file:write("nil --[[function could not be dumped]]");
					end
				end
			end;
		["thread"] = function (file, item)
				file:write("nil --[[thread]]\n");
			end;
		["userdata"] = function (file, item)
				file:write("nil --[[userdata]]\n");
			end;
	}
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
			dest[k] = v
		else
			dest[k] = v
		end
	end
	return dest
end

function mergeIntoWithCopy(dest, source)
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

function deepMergeInto(dest, source)
	for k, v in pairs(source) do
		if type(v) == "table" then 
			if type(v.copy) == "function" then
				dest[k] = v:copy()
			else
				dest[k] = deepCopy(v)
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

ClassRegistry = {
	names = {},
	classes = {}
}
setmetatable(ClassRegistry, {
	__call = function(registry, class, name)
		if name == nil then
			for key, value in pairs(_G) do
				if value == class then
					name = key
					break
				end
			end
		end
		if name then
			registry.names[name] = class
			registry.classes[class] = name
		else
			Error:throw("In ClassRegistry, no name found for class (not in global table?)")
		end
	end
})

function ClassRegistry:get(name)
	return self.names[name]
end

function ClassRegistry:className(class)
	if class == nil then return nil end
	return self.classes[class]
end

Class = (function()
	-- Object functions
	local function implements(class, ...)
		if class._interfaces == nil then class._interfaces = {} end
		
		for _, interface in ipairs(arg) do
			table.insert(class._interfaces, interface)
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
		o._class = class
		o._base = class
		o._interfaces = class._interfaces
		o.copy = objectCopy
		return o
	end
	
	local function instantiate(class, ...)
		local o = class:new()
		o:init(...)
		return o
	end
	
	local function register(class, name)
		ClassRegistery[name] = class
		return class
	end
	
	function Class(base, initialization)
		local class = {}
		
		class._class = class
		class._base = base
		class.new = new
		class.is_a = is_a
		class.like_a = like_a
		class.implements = implements
		class.register = register
		
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
	local msg = "Abstract method called in "..tostring(self)
	local name = ClassRegistry:className(self)
	if name then
		msg = msg .. ".\nClass name: "..name
	end
	Error:throw(msg)
end

function optionalFunction(self)

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
--- PERSISTENCE
---
--- Save/loading data from files
---========================================---

-- Interface Persistence
Persistence = Interface(nil, {
	save = abstractFunction, -- (object)
	load = abstractFunction, -- ()
})
-- End Persistence

-- Interface Persistent
Persistent = Interface(nil, {
	write = abstractFunction, -- (persistence)
	read = abstractFunction, -- (persistence)
})
-- End Persistent

-- Abstract Class BasicPersistence
BasicPersistence = Class(nil, {
	file = nil,
	-- Private helper functions
	actualSave = abstractFunction, -- (object)
	actualLoad = abstractFunction, -- ()
})

function BasicPersistence:init(filename)
	self.filename = filename
end

function BasicPersistence:open(filename)
	return self(filename)
end

function BasicPersistence:save(object)
	self.file = io.open(self.filename, "w")
	self:actualSave(object)
	self.file:close()
	self.file = nil
end

function BasicPersistence:load()
	self.file = io.open(self.filename, "r")
	local object = self:actualLoad()
	self.file:close()
	self.file = nil
	return object
end
-- End BasicPersistence

-- Class PoloPersistence
-- Save and load "plain old lua objects"
POLOPersistence = Class(BasicPersistence)

function POLOPersistence:save(object)
	--DEBUG("Saving...")
	--local polo = self:toPOLO(object)
	--persistence.store(self.filename, polo)
end

function POLOPersistence:load()
	--return persistence.load(self.filename)
end

--[[
function POLOPersistence:actualSave(object)
	local polo = self:toPOLO(object)
	DEBUG("PICKLE")
	--local saveString = pickle(polo)
	--self.file:write(saveString.."\n")
	persistence.store(self.file, polo)
end

function POLOPersistence:actualLoad()
	--local saveString = self.file:read("*all")
	--local polo = unpickle(saveString)
	--local object = self:fromPOLO(polo)
	--return object
	return persistence.load(self.file)
end]]

function POLOPersistence:toPOLO(object)
	if type(object) ~= "table" then
		if type(object) == "function" then
			return nil
		end
		return object
	end
	
	local polo
	local class = object._class
	local className = ClassRegistry:className(class)
	if className ~= nil then
		if class:like_a(Persistent) then
			local handle = POLOPersistence(filename)
			handle.activeClass = class
			handle.polo = {}
			object:write(handle)
			if #handle.polo == 1 then
				polo = handle.polo[1]
			else
				polo = handle.polo
				polo._inArray = true
			end
			
			polo._poloClass = className
			return polo
		else
			polo = {}
			polo._poloClass = className
		end
	else
		polo = {}
	end
	
	for key, value in pairs(object) do
		local ktype = type(key)
		if (ktype == "string" or ktype == "number")
				and not string.match(key, '^_') then
			polo[key] = self:toPOLO(value)
		end
	end
	return polo
end

function POLOPersistence:fromPOLO(polo)
	local object
	if type(polo) ~= "table" then
		return polo
	end
	
	if polo._poloClass ~= nil then
		local className = polo._poloClass
		local class = ClassRegistry:get(className)
		if type(class.readObject) == "function" then
			local handle = POLOPersistence(filename)
			handle.activeClass = class
			handle.polo = polo
			handle.poloIndex = 0
			handle.inArray = polo._inArray
			return class:readObject(handle)
		end
		object = class:new()
	else
		object = {}
	end
	
	for key, value in pairs(polo) do
		if key ~= _poloClass then
			object[key] = self:fromPOLO(value)
		end
	end
	return object
end

-- write, read, and done are for persistent objects
-- to pass their data into POLOPersistence
function POLOPersistence:write(object)
	table.insert(self.polo, self:toPOLO(object))
end

function POLOPersistence:read()
	if self.inArray then
		self.poloIndex = self.poloIndex+1
		return self.polo[self.poloIndex]
	else
		return self.polo
	end
end

function POLOPersistence:done()
	return self.poloIndex == #self.polo
end

-- End PoloPersistence

---========================================---
--- PERSISTENCE END
---========================================---

---========================================---
--- GAME
---
--- Functions to get key game specific values (sprites, etc.)
---========================================---

-- Helper Functions
local u8  = mainmemory.read_u8
local s8  = mainmemory.read_s8
local u16 = mainmemory.read_u16_le
local s16 = mainmemory.read_s16_le
local u24 = mainmemory.read_u24_le
local s24 = mainmemory.read_s24_le

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
	
	self.marioXSpeed = s8(0x7B) + u8(0x7A)/0x100
	self.marioYSpeed = s8(0x007D)
	
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
ClassRegistry(IORegistry)

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
	--local names = spec:getNames()
	local data = spec.data
	if #data <= 1 then
		if #data == 1 then
			data = data[1]
		end
		local param = data.param
		ioObject = loc[data.name]()
		if param then ioObject:setParam(param) end
		return ioObject
	end
	
	for _, datum in ipairs(data) do
		local name = datum.name
		local param = datum.param
		ioSet[name] = loc[name]()
		if param then ioSet[name]:setParam(param) end
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
	size = 0,
	partSize = {}
})
ClassRegistry(IOConfig)
-- Data format is list of:
--         { name = ?, id = ?, [param = ?] }
--		OR { name = ?, range = {?, ?}, [param = ?] }
--		OR { name = ?, range = ?, [param = ?] }

function IOConfig:init(data, names)
	self.data = data
	self.names = names
	self.layout = {}
	
	if names == nil then
		self.names = {}
		local foundNames = {}
		-- Automatically find names from data
		for _, datum in ipairs(data) do
			local name = datum.name
			if foundNames[name] == nil then
				table.insert(self.names, name)
				foundNames[name] = 1
			end
		end
	end
	
	self.size = 0
	
	for _, data in ipairs(data) do
		local name, id, range = data.name, data.id, data.range
		local inputSize = 0
		if data.id ~= nil then
			inputSize = 1
		elseif data.range~= nil then
			if type(data.range) == "table" then
				inputSize = data.range[2]-data.range[1]+1
			else
				inputSize = data.range
			end
		end
		self.layout[name] = { size = inputSize, offset = self.size+1 }
		self.size = self.size + inputSize
	end
end

function IOConfig:copy(other)
	return IOConfig(other.data, other.names)
end

function IOConfig:getData()
	return self.data
end

function IOConfig:getNames()
	return self.names
end

function IOConfig:getSize()
	return self.size
end

function IOConfig:layoutInfo(name)
	return self.layout[name]
end

function IOConfig:iterator(ioObject)
	local dataIndex = 1
	local range = nil
	local rangeIndex = 1

	--writeTableToConsole(self.data)
	return function()
		local data
		local name
		local id, handle
		local nextFound = false
		
		while not nextFound do
			if dataIndex > #self.data then
				return nil -- Done with iteration
			end
			data = self.data[dataIndex]
			name = data.name
			
			if range ~= nil then
				if rangeIndex > range[2] then
					range = nil
					dataIndex = dataIndex + 1
				else
					-- rangeIndex is next id
					id = rangeIndex
					nextFound = true
				end
				rangeIndex = rangeIndex + 1
			else
				if data.id ~= nil then
					-- Set id then increment
					id = data.id
					dataIndex = dataIndex + 1
					nextFound = true
				elseif data.range ~= nil then
					-- Initial range then
					-- return on next pass of loop
					-- (Avoid 0 length range issues)
					if type(data.range) == "table" then
						range = data.range
						rangeIndex = range[1]
					else
						range = {1, data.range}
						rangeIndex = range[1]
					end
				else
					--range = {1, ioObject.size}
					Error:throw("Invalid IOConfig")
					return nil
				end
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
ClassRegistry(IOMap)

function IOMap:init(set)
	self.set = {}
	
	local totalSize = 0
	for name, ioClass in pairs(set) do
		local size = ioClass.size
		self.ioSizes[name] = size
		self.offsets[name] = totalSize
		
		local data = { name = name, ioClass = ioClass, offset = totalSize}
		for i = totalSize+1, totalSize+size do
			self.map[i] = data
		end
		totalSize = totalSize + size
		
		table.insert(self.set, ioClass)
	end
	
	self.size = totalSize
end

function IOMap:getHandle(name, index)
	local offset = self.offsets[name]
	return offset+index
end

function IOMap:sizeOf(name)
	return self.ioSizes[name]
end
	
function IOMap:getAdjustedData(id)
	local data = self.map[id]
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
	param = nil,
	size = 0, -- Number of inputs
	setParam = optionalFunction, -- ()
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
ClassRegistry(InputMap)

function InputMap:setParam(param)
	self.param = param
end

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
	size = InputSize,
}):implements(Input)
ClassRegistry(TiledInput)

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
end

function TiledInput:get(id)
	return self.sensed[id]
end

IORegistry:registerInput("TiledInput", TiledInput)
-- End TiledInput

-- Class BiasInput
BiasInput = Class(nil, {
	size = 1,
}):implements(Input)
ClassRegistry(BiasInput)

function BiasInput:prepare()

end

function BiasInput:get(id)
	return 1
end

IORegistry:registerInput("BiasInput", BiasInput)
-- End BiasInput

-- Class VelocityInput
VelocityInput = Class(nil, {
	xFactor = 1/20,
	yFactor = 1/60,
	size = 2,
}):implements(Input)
ClassRegistry(VelocityInput)

function VelocityInput:prepare()
	currentGame:readValues()
end

function VelocityInput:get(id)
	if id == 1 then
		return currentGame.marioXSpeed*self.xFactor
	else
		return currentGame.marioYSpeed*self.yFactor
	end
end

IORegistry:registerInput("VelocityInput", VelocityInput)
-- End VelocityInput

-- XYInput
XYInput = Class(nil, {
	coordinates = {},
	sensed = {},
	size = 0,
}):implements(Input)
ClassRegistry(XYInput)

function XYInput:setParam(param)
	self.coordinates = param
	self.size = #param
end

function XYInput:prepare()
	currentGame:readValues()
   
	local sprites = currentGame:getSprites()
	local extended = currentGame:getExtendedSprites()

	local inputs = self.sensed
	
	for index, coord in ipairs(self.coordinates) do
		local x = coord[1]
		local y = coord[2]
		inputs[index] = 0
		
		local tile = currentGame:getTile(x, y)
		if tile == 1 and marioY+y < 0x1B0 then
				inputs[index] = 1
		end
		
		for i = 1,#sprites do
			distx = math.abs(sprites[i]["x"] - (marioX+x))
			disty = math.abs(sprites[i]["y"] - (marioY+y))
			if distx <= 8 and disty <= 8 then
					inputs[index] = -1
			end
		end

		for i = 1,#extended do
			distx = math.abs(extended[i]["x"] - (marioX+x))
			disty = math.abs(extended[i]["y"] - (marioY+y))
			if distx < 8 and disty < 8 then
					inputs[index] = -1
			end
		end
	end
end

function XYInput:get(id)
	return self.sensed[id]
end

IORegistry:registerInput("XYInput", XYInput)
-- End XYInput

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
	setParam = optionalFunction, -- ()
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
ClassRegistry(OutputMap)

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
ClassRegistry(ButtonOutput)

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
	
	local buttonNames = currentGame.buttonNames
	for o=1,#buttonNames do
		local button = buttonNames[o]
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
ClassRegistry(Neuron)

function Neuron:init(class, index, param)
	self.incoming = {}
	
	self.class = class
	self.ioIndex = index
	self.param = param
end

function Neuron:makeInput(index, param)
	return Neuron(self.NEURON_INPUT, index, param)
end

function Neuron:makeOutput(index, param)
	return Neuron(self.NEURON_OUTPUT, index, param)
end

function Neuron:makeHidden()
	return Neuron(self.NEURON_HIDDEN)
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

function Neuron:isInput() return self.class == self.NEURON_INPUT end
function Neuron:isOutput() return self.class == self.NEURON_OUTPUT end
function Neuron:isHidden() return self.class == self.NEURON_HIDDEN end
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
ClassRegistry(NeuralNetwork)

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
	addMap[Neuron.NEURON_INPUT] = "addInput"
	addMap[Neuron.NEURON_HIDDEN] = "addHidden"
	addMap[Neuron.NEURON_OUTPUT] = "addOutput"
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

-- Interface Executor
-- Network executor
Executor = Interface(Filter, {
	source = {},
	sink = {},
})
-- End Interface

function sigmoid(x)
        return 2/(1+math.exp(-4.9*x))-1
end

-- Class NNExecutor
NNExecutor = Class(nil, {
	values = {}, -- Saved states of the NN Neurons
	transfer = sigmoid,
	
	source = {},
	sink = {},
	neuralNetwork = {},
}):implements(Executor)
ClassRegistry(NNExecutor)

function NNExecutor:init(nn, source, sink)
	values = {}
	self.neuralNetwork = nn
	self.source = source
	self.sink = sink
end

function NNExecutor:copy(other)
	local o = self()
	
	shallowCopyValues(o, other, {"source", "sink", "neuralNetwork", "transfer"})
	copyValues(o, other, {"values"})
	return o
end

function NNExecutor:setNeuronValuePrivate(neuron, value)
	self.values[neuron.id] = value
	neuron.value = value
end

function NNExecutor:reset()
	for _, neuron in ipairs(self.neuralNetwork.allNeurons) do
		self:setNeuronValuePrivate(neuron, 0)
	end
end

function NNExecutor:processNeuron(neuron)
	local sum = 0
	for _, link in ipairs(neuron.incoming) do
		local sourceId = link.source
		sum = sum + link.weight * self.values[sourceId]
	end
   
	if #neuron.incoming > 0 then
		self:setNeuronValuePrivate(neuron, self.transfer(sum))
	end
end

function NNExecutor:run()
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
-- End NNExecutor

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
}):implements(Persistent)
ClassRegistry(Genome)

Genome.newId = makeUniqueIdGenerator()

function Genome:init(name)
	self.name = name
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

function Genome:write(persistence)
	-- Don't save the computed configuration
	local savedVariables = {}
	for _, key in ipairs(self.savedKeys) do
		savedVariables[key] = self[key]
	end
	persistence:write(savedVariables)
end

function Genome:read(persistence)
	local savedVariables = persistence:read(savedVariables)
	local savedGenome = self:new()
	for _, key in ipairs(self.savedKeys) do
		savedGenome[key] = savedVariables[key]
	end
	return savedGenome
end

function Genome:getInputSpec()
	return self.inputSpec
end

function Genome:getOutputSpec()
	return self.outputSpec
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
	savedKeys = {"genes", "fitness", "id", "history", "innovation", "inputSpec", "outputSpec", "mutationRates", "maxneuron"}
})
ClassRegistry(NNGenome)

function NNGenome:init()
	self.genes = {}
	self.fitness = 0
	self.adjustedFitness = 0
	self.network = {}
	self.maxneuron = 0
	self.neuronCount = 0
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

function NNGenome:newNeuron()
	self.neuronCount = self.neuronCount+1
	return self.neuronCount
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
	return NNExecutor(config, source, sink)
end

function NNGenome:difference(other)
	local dd = DeltaDisjoint*self:disjoint(self.genes, other.genes)
	local dw = DeltaWeights*self:weights(self.genes, other.genes)
	return dd + dw
end

function NNGenome:neuronIdForGene(geneEndPoint)
	return self.geneNeuronMap[geneEndPoint]
end

function NNGenome:randomNeuron(nonInput)
	local genes = self.genes
	
	local numInputs = self.inputSpec:getSize()
	
	local count = self.maxneuron
	if nonInput then count = count - numInputs end
	local n = math.random(1, count)
	if nonInput then n = n + numInputs end
	
	return n
end
 
function NNGenome:containsLink(link)
	local genes = self.genes
	for _, gene in ipairs(genes) do
		if gene.into == link.into and gene.out == link.out then
			return true
		end
	end
	return false
end
 
function NNGenome:disjoint(genes1, genes2)
	local genePair = { genes1, genes2 }
	local innovationPair = { {}, {} }
	
	for i = 1, 2 do
		for _, gene in ipairs(genePair[i]) do
			innovationPair[i][gene.innovation] = true
		end
	end
	
	local disjointGenes = 0
	for i = 1, 2 do
		local otherI = 3 - i
		for _, gene in ipairs(genePair[i]) do
			if not innovationPair[otherI][gene.innovation] then
				disjointGenes = disjointGenes + 1
			end
		end
	end
	
	local maxGeneCount = math.max(#genes1, #genes2)
	return disjointGenes/maxGeneCount
end
 
function NNGenome:weights(genes1, genes2)
	local innovationMap2 = {}
	for _, gene2 in ipairs(genes2) do
		innovationMap2[gene2.innovation] = gene2
	end

	local total = 0
	local coincident = 0
	for _, gene in ipairs(genes1) do
		if innovationMap2[gene.innovation] ~= nil then
			local gene2 = innovationMap2[gene.innovation]
			total = total + math.abs(gene.weight - gene2.weight)
			coincident = coincident + 1
		end
	end
   
	return total / coincident
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
ClassRegistry(SimpleMutation)

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
ClassRegistry(MutationWithRate)
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

-- Interface IBreeder
IBreeder = Interface(nil, {
	addMutation = abstractFunction,
	addParam = abstractFunction,
	breed = abstractFunction
})
-- End IBreeder

-- Abstract Class Breeder
Breeder = Class(nil, {
	param = {},
	mutations = {}
}):implements(IBreeder)
ClassRegistry(Breeder)

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
			if mutation then
				mutation.param[key] = v
			end
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

function Breeder:breed(organism, organisms)
	local genome = organism.genome

	local pool = {}
	for _, organism in ipairs(organisms) do
		table.insert(pool, organism.genome)
	end
	local child = self:reproduce(genome, pool)
	self:mutate(child)
	return child
end
-- End Breeder

-- Class NNBreeder
NNBreeder = Class(Breeder, {
	mutations = {}
})
ClassRegistry(NNBreeder)

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
	local neuron1 = genome:randomNeuron(false)
	local neuron2 = genome:randomNeuron(true)  
	
	if not neuron1 or not neuron2 then return end
	local newLink = NNGenome:makeGene()
	local numInputs = genome.inputSpec:getSize()
	
	if neuron1 <= numInputs and neuron2 <= numInputs then
			--Both input nodes
			return
	end
	if neuron2 <= numInputs then
			-- Swap output and input
			local temp = neuron1
			neuron1 = neuron2
			neuron2 = temp
	end

	newLink.into = neuron1
	newLink.out = neuron2
	if param.forceType then
		local layout = genome.inputSpec:layoutInfo(param.forceType)
		if layout then
			newLink.into = math.random(layout.offset, layout.offset+layout.size-1)
		end
	end
   
	if genome:containsLink(newLink) then
			return
	end
	newLink.innovation = newInnovation()
	newLink.weight = math.random()*4-2
  
	table.insert(genome.genes, newLink)
end
NNBreeder:addMutation("link",
	MutationWithRate(linkMutation):addParam({forceType = nil}))
NNBreeder:addMutation("bias",
	MutationWithRate(linkMutation):addParam({forceType = "BiasInput"}))
NNBreeder:addMutation("vel",
	MutationWithRate(linkMutation):addParam({forceType = "VelocityInput"}))
	
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
	--g1 = g1.genome
	--g2 = g2.genome
	if g2.fitness > g1.fitness then
			tempg = g1
			g1 = g2
			g2 = tempg
	end

	local child = NNGenome()
	child.inputSpec = g1.inputSpec:copy()
	child.outputSpec = g1.outputSpec:copy()
	
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
	-- Add xy for XYInput
	local mutationTypes = {"connections", "link", "bias",
		"node", "enable", "disable", "xy"}
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

-- Class NNXYBreeder
NNXYBreeder = Class(NNBreeder)

function xyMutation(param, genome)
	if #genome.genes == 0 then
			return
	end

	local usedXYInputs = {}
	local usedXYHash = {}
	local layout = genome.inputSpec:layoutInfo("XYInput")
	for _, gene in ipairs(genome.genes) do
		if gene.enabled then
			local id = gene.into
			if id >= layout.offset and id < layout.offset+layout.size then
				if not usedXYHash[id] then
					usedXYHash[id] = true
					table.insert(usedXYInputs, id-layout.offset+1)
				end
			end
		end
	end
	
	if #usedXYInputs == 0 then return end
	
	genome.inputSpec.data = deepCopy(genome.inputSpec.data)
	local pointIndex = usedXYInputs[math.random(1,#usedXYInputs)]
	
	local dx = math.random(-XYDistance, XYDistance)
	local dy = math.random(-XYDistance, XYDistance)
	
	local xyInput
	for _, input in ipairs(genome.inputSpec.data) do
		if input.name == "XYInput" then
			xyInput = input
		end
	end
	
	local point = xyInput.param[pointIndex]
	point[1] = point[1] + dx
	point[2] = point[2] + dy
end
NNXYBreeder:addMutation("xy",
	MutationWithRate(xyMutation))
-- End NNXYBreeder

---========================================---
--- BREEDER END
---========================================---

---========================================---
--- ORGANISM
---
--- Acts and breed
--- Interface between the genome and the simulation
---========================================---

-- Interface Organism
Organism = Interface(nil,{
	name = "?",
	birth = abstractFunction, -- ()
	act = abstractFunction,	-- ()
	breed = abstractFunction, -- (species)
	die = abstractFunction, -- () for cleanup
	difference = abstractFunction, -- for species
})
-- End Organism

-- Abstract Class BasicOrganism
BasicOrganism = Class(nil, {
	genome = {},
	phenotype = {},
	breeder = nil,
	executor = nil,
	fitness = 0,
	savedKeys = {"genome", "fitness", "name"},
	breederType = nil,
	executorType = nil
}):implements(Organism, Persistent)
ClassRegistry(BasicOrganism)

function BasicOrganism:init(genome)
	self.genome = genome
	self.name = genome.name
	self.fitness = 0
	self.globalRank = 0
end

function BasicOrganism:write(persistence)
	-- Don't save the computed configuration
	local savedVariables = {}
	for _, key in ipairs(self.savedKeys) do
		savedVariables[key] = self[key]
	end
	savedVariables.executorName = ClassRegistry:className(self.executorType)
	savedVariables.breederName = ClassRegistry:className(self.breederType)
	persistence:write(savedVariables)
end

function BasicOrganism:read(persistence)
	local savedVariables = persistence:read(savedVariables)
	local savedObject = self:new()
	for _, key in ipairs(self.savedKeys) do
		savedObject[key] = savedVariables[key]
	end
	savedObject.executorType = ClassRegistry:get(savedObject.executorName)
	savedObject.breederType = ClassRegistry:get(savedObject.breederName)
	return savedObject
end

function BasicOrganism:setExecutor(executorType)
	self.executorType = executorType
end

function BasicOrganism:setBreeder(breederType)
	self.breederType = breederType
end

function BasicOrganism:birth()	
	local source, sink = self:makeIO()
	self.source, self.sink = source, sink
	--self.phenotype = self.genome:getConfig(source, sink)
	self.phenotype = self:makePhenotype(source, sink)
	self.executor = self:makeExecutor(self.phenotype, source, sink)
	self.executor:reset()
end

function BasicOrganism:makeIO()
	local inputSpec = self.genome:getInputSpec()
	local outputSpec = self.genome:getOutputSpec()
	return IORegistry:getInput(self.genome.inputSpec),
		IORegistry:getOutput(self.genome.outputSpec)
end

function BasicOrganism:act()
	self.source:prepare()
	self.sink:prepare()
	self.executor:run()
end

function BasicOrganism:breed(species)
	self.breeder = self:makeBreeder()
	local childGenome = self.breeder:breed(self, species)
	local child = self._class(childGenome)
	child:setExecutor(self.executorType)
	child:setBreeder(self.breederType)
	return child
end

function BasicOrganism:mutate()
	self.breeder = self:makeBreeder()
	self.breeder:mutate(self.genome)
	return self.genome
end

function BasicOrganism:die()
	self.executor = nil
	self.phenotype = nil
end

function BasicOrganism:difference(organism2)
	return self.genome:difference(organism2.genome)
end

function BasicOrganism:getGenome()
	return self.genome
end
-- End BasicOrganism

-- Class NNOrganism
NNOrganism = Class(BasicOrganism, {
	
})
ClassRegistry(NNOrganism)
-- End NNOrganism

function NNOrganism:makePhenotype(source, sink)
	local network = NeuralNetwork()
	
	local id
	local genome = self.genome
	genome.geneNeuronMap = {}
	local idMap = genome.geneNeuronMap
	
	local neuronCount = 1
	for name, id, handle in genome.inputSpec:iterator(source) do
		id = network:addNeuron(Neuron:makeInput(handle)).id
		idMap[neuronCount] = id
		neuronCount = neuronCount + 1
	end

	for name, id, handle in genome.outputSpec:iterator(sink) do
		id = network:addNeuron(Neuron:makeOutput(handle)).id
		idMap[neuronCount] = id
		neuronCount = neuronCount + 1
	end
	
	table.sort(genome.genes, function (a,b)
			return (a.out < b.out)
	end)
	
	for i=1,#genome.genes do
		local gene = genome.genes[i]
		
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
	
	self.network = network
	return network
end

function NNOrganism:makeExecutor(phenotype, source, sink)
	return self.executorType(phenotype, source, sink)
end

function NNOrganism:makeBreeder()
	return self.breederType():addParam({crossoverChance = CrossoverChance})
end

---========================================---
--- ORGANISM END
---========================================---

---========================================---
--- MOTHER
---
--- Organism factory
--- Creates the first set of organisms
---========================================---

-- Interface Mother
Mother = Interface(nil, {
	makeOriginal = abstractFunction
})
-- End Mother

-- Class BasicMother
BasicMother = Class(nil, {
	inputSpec = {},
	outputSpec = {},
	genes = {},
	genomeType = {},
	executorType = {},
	breederType = {},
	organismType = {}
}):implements(Mother)

function BasicMother:setSpecHelper(spec, ioGetterFunc)
	local finalSpec = {}
	for _, data in ipairs(spec) do
		local dataType = type(data)
		local correctData
		if dataType ~= "table" or (data.id == nil and data.range == nil) then
			local name
			if dataType == "table" then
				name = data.name
			else
				name = dataType
			end
			
			local ioConnector = ioGetterFunc( IORegistry,
				{ data = { name = name, param = data.param }} )
			local size = ioConnector.size
			correctData = { name = name, range = size, param = data.param }
		else
			correctData = data
		end
		table.insert(finalSpec, correctData)
	end
	return finalSpec
end

-- Note that the "spec" here is looser than that required
-- for IOConfig for convenience
function BasicMother:setInput(spec)
	-- Fix any unspecified sizes
	
	local finalSpec = self:setSpecHelper(spec, IORegistry.getInput)
	self.inputSpec = IOConfig(finalSpec)
	return self
end

-- Note that the "spec" here is looser than that required
-- for IOConfig for convenience
function BasicMother:setOutput(spec)
	-- Fix any unspecified sizes
	
	local finalSpec = self:setSpecHelper(spec, IORegistry.getOutput)
	self.outputSpec = IOConfig(finalSpec)
	return self
end

function BasicMother:setOrganismType(typeName)
	self.organismType = ClassRegistry:get(typeName)
	return self
end

function BasicMother:setGenomeType(typeName)
	self.genomeType = ClassRegistry:get(typeName)
	return self
end

function BasicMother:setExecutor(typeName)
	self.executorType = ClassRegistry:get(typeName)
	return self
end

function BasicMother:setBreeder(typeName)
	self.breederType = ClassRegistry:get(typeName)
	return self
end

function BasicMother:setGenes(genes)
	self.genes = genes
	return self
end

function BasicMother:makeOriginal()
	local genome = self.genomeType()
	genome = deepMergeInto(genome, {
		inputSpec = self.inputSpec,
		outputSpec = self.outputSpec,
		genes = self.genes,
		fitness = 0,
		adjustedFitness = 0,
		network = {},
		globalRank = 0,
		mutationRates = {
			connections = MutateConnectionsChance,
			link = LinkMutationChance,
			bias = BiasMutationChance,
			vel = VelMutationChance,
			node = NodeMutationChance,
			enable = EnableMutationChance,
			disable = DisableMutationChance,
			step = StepSize,
			-- XYInput mutation
			xy = XYMutationChance
		},
		geneNeuronMap = {}
	})
	genome.maxneuron = genome.inputSpec:getSize() + genome.outputSpec:getSize()
	local originalOrganism = self.organismType(genome)
	originalOrganism:setExecutor(self.executorType)
	originalOrganism:setBreeder(self.breederType)
	originalOrganism:mutate()
	return originalOrganism
end
-- End BasicMother

-- Class NNMother
NNMother = Class(BasicMother, {
	genomeType = NNGenome,
	executorType = NNExecutor,
	breederType = NNBreeder,
	organismType = NNOrganism
})
-- End NNMother

---========================================---
--- MOTHER END
---========================================---

---========================================---
--- SPECIES
---
--- Data for groups of similar organisms
---========================================---

-- Class Species
Species = Class(nil, {
	organisms = {},
	name = "?"
})
ClassRegistry(Species)

function Species:init(name)
	self.name = name
	self.topFitness = 0
	self.staleness = 0
	self.organisms = {}
	self.averageFitness = 0
end

function Species:calculateAverageFitness()
	local total = 0

	for _, organism in ipairs(self.organisms) do
		total = total + organism.globalRank
	end

	self.averageFitness = total / #self.organisms
end

function Species:addOrganism(organism)
	table.insert(self.organisms, organism)
end

function Species:organismIter()
	local i = 0
	return function ()
		i = i + 1
		return self.organisms[i], i
	end
end
-- End Species

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
	organisms = {},
	maxFitness,
})
ClassRegistry(Population)

function Population:init(maxSize)	
	self.species = {}
	self.generation = 0
	self.maxFitness = 0
	
	self.maxSize = maxSize
end

function Population:addOrganism(organism)
	local foundSpecies = false
	
	for s=1,#self.species do
		local species = self.species[s]
		if not foundSpecies and self:isInSameSpecies(organism, species.organisms[1]) then
			species:addOrganism(organism)
			organism.name = #species.organisms
			foundSpecies = true
		end
	end
   
	if not foundSpecies then
			local childSpecies = Species(#self.species+1)
			childSpecies:addOrganism(organism)
			table.insert(self.species, childSpecies)
			organism.name = 1
	end
	
	table.insert(self.organisms, organism)
end

function Population:speciesIter()
	local i = 0
	return function()
		i = i + 1
		return self.species[i], i
	end
end

function Population:isInSameSpecies(organism1, organism2)
		local diff = organism1:difference(organism2)
        return diff < DeltaThreshold
end

function Population:rankGlobally()
        local global = {}
        for s = 1,#self.species do
                local species = self.species[s]
                for g = 1,#species.organisms do
                        table.insert(global, species.organisms[g])
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
              
                table.sort(species.organisms, function (a,b)
                        return (a.fitness > b.fitness)
                end)
               
                local remaining = math.ceil(#species.organisms/2)
                if cutToOne then
                        remaining = 1
                end
                while #species.organisms > remaining do
                        table.remove(species.organisms)
                end
        end
end

function Population:breedChild(species)
	local child = {}
	local organism = species.organisms[math.random(1, #species.organisms)]
	
	return organism:breed(species.organisms)
end

function Population:removeStaleSpecies()
        local survived = {}
 
        for s = 1,#self.species do
                local species = self.species[s]
               
                table.sort(species.organisms, function (a,b)
                        return (a.fitness > b.fitness)
                end)
                if species.organisms[1].fitness > species.topFitness then
                        species.topFitness = species.organisms[1].fitness
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
                self:addOrganism(child)
        end
       
        self.generation = self.generation + 1
       
		local filename = ConfigUI:getSaveLoadFile()
        self:writeFile("backup." .. self.generation .. "." .. filename)
end

function Population:writeFile(filename)
	local persistence = POLOPersistence(filename)
	self = persistence:save(self)
end
 
function Population:savePool()
	local filename = ConfigUI:getSaveLoadFile()
	self:writeFile(filename)
end

function Population:loadFile(filename)
	local persistence = POLOPersistence(filename)
	newPop = persistence:load()
	
	popSim:init(newPop, nil)
	while popSim:fitnessAlreadyMeasured() do
			popSim:nextGenome()
	end
end

function Population:loadPool()
	local filename = ConfigUI:getSaveLoadFile()
	self:loadFile(filename)
end

function Population:makeStarter(mother)
	local pop = Population(PopulationMax)
	
	for i=1,PopulationMax do
		pop:addOrganism(mother:makeOriginal())
	end

	return pop
end

function Population:countMeasured()
	local measured = 0
	local total = 0
	for _,species in pairs(self.species) do
		for _,organism in pairs(species.organisms) do
			total = total + 1
			if organism.fitness ~= 0 then
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
ClassRegistry(BasicJudge)

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
ClassRegistry(RightmostJudge)

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
	local rightmostBonusFactor = 1
	local fitness = math.floor(rightmostBonusFactor*self.rightmost -  frame / 2 - self.timeout/2)
	return fitness
end

function RightmostJudge:finalMeasurement()
	local frame = self.frame - 1
	local rightmostBonusFactor = 1
	local fitness = rightmostBonusFactor*self.rightmost - frame / 2
	
	if self.rightmost > currentGame.rightEnd then
		fitness = fitness + 1000
	end
	if fitness == 0 then
			fitness = -1
	end
	return fitness
end

function RightmostJudge:shouldEnd()
	local fitness = self:measure()
	return self.timeout <= 0 or fitness < -100
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
ClassRegistry(BasicSimulation)

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
ClassRegistry(GenomeSimulation)

function GenomeSimulation:init(organism, evaluator)
	self:_baseInit()
	
	self.done = false
	
	self.organism = organism

	--self.organism = NNOrganism(genome)
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
	
	--local organism = genome:makeOrganism(IORegistry)
	--organism:reset()
	self.organism:birth()
	
	self.evaluator:setOrganism(self.organism)
	self.evaluator:setup()
	
	self:runEvaluation()
end

function GenomeSimulation:isDone()
	return self.evaluator:shouldEnd()
end

function GenomeSimulation:runEvaluation()
	self.organism:act()
end

function GenomeSimulation:evaluate()
	if self.frame%5 == 0 then
		self:runEvaluation()
	end
	self.evaluator:step()
end

function GenomeSimulation:finish()

	local fitness = self.evaluator:finalMeasurement()
	self.organism.fitness = fitness
	self.fitness = fitness
	
	-- TODO: Don't reference popSim
	local pop = popSim.population
	if fitness > pop.maxFitness then
			pop.maxFitness = fitness
			ConfigUI:setMaxFitness(pop.maxFitness)
			
			local filename = ConfigUI:getSaveLoadFile()
			pop:writeFile("backup." .. pop.generation .. "." .. filename)
	end
	
	self.organism:die()
end

function GenomeSimulation:getCurrentFitness()
	return self.evaluator:measure()
end
-- End GenomeSimulation

-- PopulationSimulation
PopulationSimulation = Class(BasicSimulation)
ClassRegistry(PopulationSimulation)

function PopulationSimulation:init(population, genomeSimFactory)
	self:_baseInit()

	self.population = population
	self.genomeSimFactory = genomeSimFactory
	self.genomeSimulation = nil
	self.genomeSimDone = false
	
	self.speciesIter = nil
	self.organismIter = nil
	self.activeSpecies = nil
	self.activeOrganism = nil
	self:reset()
	
	return self
end

function PopulationSimulation:isDone()
	return self.activeSpecies == nil
end

function PopulationSimulation:genomeSimDoneHandler(sim)
	self.genomeSimDone = true
	
	console.writeline("Gen " .. self.population.generation .. " species " .. self.activeSpecies.name .. " genome " .. self.activeOrganism.name .. " fitness: " .. sim.fitness)
	
	self:reset()
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
	self.speciesIter = self.population:speciesIter()
	self.activeSpecies = self.speciesIter()
	self.organismIter = self.activeSpecies:organismIter()
	self.activeOrganism = self.organismIter()

	--self.currentSpecies = 1
	--self.currentGenome = 1
end

function PopulationSimulation:finish()
	self.population:newGeneration()
	
	self:reset()
end

function PopulationSimulation:nextGenome()
	local population = self.population
	--[[
	self.currentGenome = self.currentGenome + 1
	if self.currentGenome > #population.species[self.currentSpecies].genomes then
		self.currentGenome = 1
		self.currentSpecies = self.currentSpecies+1
	end]]--
	
	self.activeOrganism = self.organismIter()
	
	if self.activeOrganism == nil then
		self.activeSpecies = self.speciesIter()
		if self.activeSpecies ~= nil then
			self.organismIter = self.activeSpecies:organismIter()
			self.activeOrganism = self.organismIter()
		end
	end
end

function PopulationSimulation:fitnessAlreadyMeasured()
	local species = self.activeSpecies
	
	if species == nil then return false end
	
	local organism = self.activeOrganism
	return organism.fitness ~= 0
end

function PopulationSimulation:initializeGenomeSim()
	savestate.load("DP1.state"); -- TODO remove hardcoded
	--savestate.load(Filename);
	--rightmost = 0
	--timeout = TimeoutConstant
	JoypadUtil:clear()
	
	local organism = self.activeOrganism

	local genomeSimulation = GenomeSimulation(organism, RightmostJudge)
	genomeSimulation:setup()
	genomeSimulation:step()
	self.genomeSimulation = genomeSimulation
	--self.genomeSimulation = genomeSimFactory(genome)
	
	self.genomeSimDone = false
	self.genomeSimulation.onComplete:add(function (sim)
		self:genomeSimDoneHandler(sim)
	end)
end
--[[
function PopulationSimulation:activeGenome()
	--local species = self.population.species[self.currentSpecies]
	--local genome = species.genomes[self.currentGenome]
	
	return self.activeGenome
end]]--

function PopulationSimulation:getCurrentSpecies()
	--return self.currentSpecies
	return self.activeSpecies.name
end

function PopulationSimulation:getCurrentGenome()
	return self.activeOrganism.name
	--return self.currentGenome
end

-- TODO: remove/fix
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
	
	for species,s in self.population:speciesIter() do
		for organism,g in species:organismIter() do
			if organism.fitness > maxfitness then
				maxfitness = organism.fitness
				self.activeSpecies = species
				self.activeOrganism = organism
			end
		end
	end
	
	self.maxFitness = maxfitness
	self.genomeSimulation = nil
	
	ConfigUI:setMaxFitness(self.maxFitness)
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
	local x, y, value, hideBorder = cell.x, cell.y, cell.value, cell.hideBorder
	
	local color = math.floor((value+1)/2*256)
	local border = 0x00000000
	
	if color > 255 then color = 255 end
	if color < 0 then color = 0 end
	local opacity = 0xFF000000
	if value == 0 then
		opacity = 0x50000000
	end
	color = opacity + color*0x10000 + color*0x100 + color
	if not hideBorder then
		border = opacity
	end
	gui.drawBox(x-2,y-2,x+2,y+2,border,color)
end

function NNUI:drawTiledInput(x, y, offset)
	local i = offset
	local network = self.network
	
	if self.needCellGeneration then
		for row = 0, 2*BoxRadius do
			for col = 0, 2*BoxRadius do
				cell = {
					x = x +2 + 5*col,
					y = y +2 + 5*row,
					neuron = network.inputNeurons[i],
					value = network.inputNeurons[i].value,
					requireInput = true,
					fixed = true
				}
				table.insert(self.inputCells, cell)
				table.insert(self.cells, cell)
				i = i + 1
			end
		end
	end
	
	gui.drawBox(x, y, x+5+2*BoxRadius*5-1, y+5+2*BoxRadius*5-1,0xFF000000, 0x80808080)
	gui.drawBox(x+29 +2,y+31+2,x+31+2,y+38+2,0x00000000,0x80FF0000)
	
	return x, y + 5+2*BoxRadius*5+5
end

function NNUI:drawBias(x, y, offset)
	local network = self.network
	if self.needCellGeneration then
		local biasCell = {
			x = x+60+2,
			y = y,
			neuron = network.inputNeurons[offset],
			value = network.inputNeurons[offset].value,
			fixed = true
		}
		table.insert(self.inputCells, biasCell)
		table.insert(self.cells, biasCell)
	end
	
	return x, y+10
end

function NNUI:drawVelocity(x, y, offset)
	local network = self.network
	if self.needCellGeneration then
		local vxCell = {
			x = x+30+2,
			y = y,
			neuron = network.inputNeurons[offset],
			value = network.inputNeurons[offset].value,
			fixed = true
		}
		table.insert(self.inputCells, vxCell)
		table.insert(self.cells, vxCell)
		
		local vyCell = {
			x = x+60+2,
			y = y,
			neuron = network.inputNeurons[offset+1],
			value = network.inputNeurons[offset+1].value,
			fixed = true
		}
		table.insert(self.inputCells, vyCell)
		table.insert(self.cells, vyCell)
	end
	
	gui.drawText(x+15, y-5, "Vx", 0xFF000000, 8)
	gui.drawText(x+45, y-5, "Vy", 0xFF000000, 8)
	return x, y+10
end

function NNUI:drawXYInput(x, y, offset)
	local i = offset
	local network = self.network
	local points = self.data.param

	if points == nil then return x, y end -- No points to draw
	if self.needCellGeneration then
		for _, point in ipairs(points) do
			local px = point[1]
			local py = point[2]
			cell = {
				x = x +32 + 5*px/16,
				y = y +32 + 5*py/16,
				neuron = network.inputNeurons[i],
				value = network.inputNeurons[i].value,
				--requireInput = true,
				hideBorder = true,
				fixed = true
			}
			table.insert(self.inputCells, cell)
			table.insert(self.cells, cell)
			i = i + 1
		end
	end
	
	gui.drawBox(x, y, x+5+2*BoxRadius*5-1, y+5+2*BoxRadius*5-1,0xFF000000, 0x80808080)
	
	-- Mario box
	gui.drawBox(x+29 +2,y+31+2,x+31+2,y+38+2,0x00000000,0x80FF0000)
	
	return x, y + 5+2*BoxRadius*5+5
end

NNUI.inputDrawers = {
	TiledInput = "drawTiledInput",
	BiasInput = "drawBias",
	VelocityInput = "drawVelocity",
	XYInput = "drawXYInput"
}

function NNUI:drawInput(x, y)
	local network = self.network
	
	if self.needCellGeneration then
		self.inputCells = {}
	end
	
	local inputConfig = self.inputConfig
	for _, data in ipairs(inputConfig:getData()) do
		self.data = data
		local name = data.name
		local layout = inputConfig:layoutInfo(name)
		local offset, size = layout.offset, layout.size
		
		local drawer = self.inputDrawers[name]
		if drawer then
			x, y = self[drawer](self, x, y, offset, size)
		end
	end
	
	for _, cell in ipairs(self.inputCells) do
		cell.value = cell.neuron.value
		-- Show unless value is zero and require input
		if cell.value ~= 0 or not cell.requireInput then
			self:drawCell(cell)
		end
	end
end

function NNUI:drawOutput(x, y)
	local network = self.network
	
	if self.needCellGeneration then
		self.outputCells = {}
		
		local i = #network.inputNeurons
		for o = 1,#network.outputNeurons do
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

function NNUI.applyBoundary(point, boundary)
	if point.x < boundary.left then
		point.x = boundary.left
	elseif point.x > boundary.right then
		point.x = boundary.right
	end
	
	if point.y < boundary.top then
		point.y = boundary.top
	elseif point.y > boundary.bottom then
		point.y = boundary.bottom
	end
end

function NNUI:organizeNetwork(count, boundary)
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
				--[[
				if c1.x < 90 then
					c1.x = 90
				end
			   
				if c1.x > 220 then
					c1.x = 220
				end--]]
				c1.y = 0.75*c1.y + 0.25*c2.y
				
				self.applyBoundary(c1, boundary)
			end
			
			if not c2.fixed then
				c2.x = 0.25*c1.x + 0.75*c2.x
				if c1.x >= c2.x then
					c2.x = c2.x + 40
				end
				--[[
				if c2.x < 90 then
					c2.x = 90
				end
				if c2.x > 220 then
					c2.x = 220
				end]]
				c2.y = 0.25*c1.y + 0.75*c2.y
				
				self.applyBoundary(c2, boundary)
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
		
		self:organizeNetwork(4, {
			left = 90, right = 210,
			top = 0, bottom = 256
		})
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

function NNUI:draw()
	local organism = popSim.activeOrganism
	--if genome.organism == nil then return end
	
	local network = organism.network
	
	if network == nil then return end
	
	if network ~= self.network then
		local inputConfig = organism.genome.inputSpec
		self.inputConfig = inputConfig
		self.needCellGeneration = true
		self.cells = {}
		self.network = network
		self.links = network:getLinks()
	else
		self.needCellGeneration = false
	end
	
	self:drawInput(20, 40)
	self:drawOutput()
	self:drawHidden()
	self:drawLinks()
end
-- End NNUI

-- Class MutationRatesUI
MutationRatesUI = Class(BasicUI)

function MutationRatesUI:draw()
	-- TODO fix encapsulation break
	local genome = popSim.activeOrganism:getGenome()
	local pos = 100
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

-- Class ConfigUI
ConfigUI = Class(nil,{
	form = nil,
	defaultFile = Filename .. ".pool"
})

function ConfigUI:show()
	local form = forms.newform(200, 260, "Fitness")
	self.form = form
	local pop = popSim.population
	
	self.maxFitnessLabel = forms.label(form, "Max Fitness: " .. math.floor(pop.maxFitness), 5, 8)
	self.showNetworkCheck = forms.checkbox(form, "Show Map", 5, 30)
	self.showMutationRatesCheck = forms.checkbox(form, "Show M-Rates", 5, 52)
	self.restartButton = forms.button(form, "Restart", function()
			popSim = PopulationSimulation(Population:makeStarter(mother), nil)
		end, 5, 77)
		
	self.saveButton = forms.button(form, "Save", function()
			popSim.population:savePool()
		end, 5, 102)
	self.loadButton = forms.button(form, "Load", function()
			popSim.population:loadPool()
		end, 80, 102)
	
	self.saveLoadFileBox = forms.textbox(form, self.defaultFile, 170, 25, nil, 5, 148)
	self.saveLoadLabel = forms.label(form, "Save/Load:", 5, 129)
	self.playTopButton = forms.button(form, "Play Top", function()
			popSim:playTop()
		end, 5, 170)
	self.hideBannerCheck = forms.checkbox(form, "Hide Banner", 5, 190)
end

function ConfigUI:dispose()
	if self.form then
		forms.destroy(self.form)
	end
end

function ConfigUI:showMutationRates()
	local form = self.form
	return form and forms.ischecked(self.showMutationRatesCheck)
end

function ConfigUI:showNetwork()
	local form = self.form
	return form and forms.ischecked(self.showNetworkCheck)
end

function ConfigUI:hideBanner()
	local form = self.form
	return form and forms.ischecked(self.hideBannerCheck)
end

function ConfigUI:getSaveLoadFile()
	local form = self.form
	if form ~= nil then
		local filename = forms.gettext(self.saveLoadFileBox)
		if filename ~= nil and filename ~= "" then
			return filename
		end
	end
	return self.defaultFile
end

function ConfigUI:setMaxFitness(fitness)
	if self.form == nil then return end
	forms.settext(self.maxFitnessLabel, "Max Fitness: " .. math.floor(fitness))
end
-- End ConfigUI

---========================================---
--- UI END
---========================================---

PopulationMax = 200
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
VelMutationChance = 0.10
StepSize = 0.1
DisableMutationChance = 0.4
EnableMutationChance = 0.2
 
TimeoutConstant = 20
 
MaxNodes = 1000000

XYMutationChance = 0.75
XYDistance = 8

newInnovation = makeUniqueIdGenerator()

mother = NNMother()
		:setInput({
			{name = "TiledInput", range = 169},
			{name = "VelocityInput", range = 2},
			{name = "BiasInput", range = 1}
		})
		:setOutput({ {name = "ButtonOutput", range = 8} })

local fullParam = {}
for i = -6, 6 do
	for j = -6, 6 do
		table.insert(fullParam, {i*16, j*16})
	end
end

motherXY = NNMother()
		:setInput({
			{name = "XYInput", param = fullParam},
			{name = "VelocityInput", range = 2},
			{name = "BiasInput", range = 1}
		})
		:setOutput({ {name = "ButtonOutput", range = 8} })
		:setBreeder(NNXYBreeder)

mother = motherXY
popSim = nil
popSim = PopulationSimulation(Population:makeStarter(mother), nil)

-- Show Form
ConfigUI:show()

event.onexit(function ()
	ConfigUI:dispose()
end)

while true do
	if ConfigUI:showNetwork() then
		NNUI:draw()
	end
   if ConfigUI:showMutationRates() then
		MutationRatesUI:draw()
	end
	
	popSim:step()
	JoypadUtil:apply()

	if not ConfigUI:hideBanner()then
		BannerUI:draw()
	end

	emu.frameadvance();
end