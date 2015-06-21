
-- Are You Smarter Than MarIO?
-- Based on SethBling's MarI/O

if gameinfo.getromname() == "Super Mario World (USA)" then
        Filename = "DP1.state"
        ButtonNames = {
                "A",
                "B",
                "X",
                "Y",
                "Up",
                "Down",
                "Left",
                "Right",
        }
elseif gameinfo.getromname() == "Super Mario Bros." then
        Filename = "SMB1-1.state"
        ButtonNames = {
                "A",
                "B",
                "Up",
                "Down",
                "Left",
                "Right",
        }
end
 
BoxRadius = 6
InputSize = (BoxRadius*2+1)*(BoxRadius*2+1)
 
Inputs = InputSize+1
Outputs = #ButtonNames
 
function getPositions()
        if gameinfo.getromname() == "Super Mario World (USA)" then
                marioX = memory.read_s16_le(0x94)
                marioY = memory.read_s16_le(0x96)
               
                local layer1x = memory.read_s16_le(0x1A);
                local layer1y = memory.read_s16_le(0x1C);
               
                screenX = marioX-layer1x
                screenY = marioY-layer1y
        elseif gameinfo.getromname() == "Super Mario Bros." then
                marioX = memory.readbyte(0x6D) * 0x100 + memory.readbyte(0x86)
                marioY = memory.readbyte(0x03B8)+16
       
                screenX = memory.readbyte(0x03AD)
                screenY = memory.readbyte(0x03B8)
        end
end
 
function getTile(dx, dy)
        if gameinfo.getromname() == "Super Mario World (USA)" then
                x = math.floor((marioX+dx+8)/16)
                y = math.floor((marioY+dy)/16)
               
                return memory.readbyte(0x1C800 + math.floor(x/0x10)*0x1B0 + y*0x10 + x%0x10)
        elseif gameinfo.getromname() == "Super Mario Bros." then
                local x = marioX + dx + 8
                local y = marioY + dy - 16
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
end
 
function getSprites()
        if gameinfo.getromname() == "Super Mario World (USA)" then
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
        elseif gameinfo.getromname() == "Super Mario Bros." then
                local sprites = {}
                for slot=0,4 do
                        local enemy = memory.readbyte(0xF+slot)
                        if enemy ~= 0 then
                                local ex = memory.readbyte(0x6E + slot)*0x100 + memory.readbyte(0x87+slot)
                                local ey = memory.readbyte(0xCF + slot)+24
                                sprites[#sprites+1] = {["x"]=ex,["y"]=ey}
                        end
                end
               
                return sprites
        end
end
 
function getExtendedSprites()
        if gameinfo.getromname() == "Super Mario World (USA)" then
                local extended = {}
                for slot=0,11 do
                        local number = memory.readbyte(0x170B+slot)
                        if number ~= 0 then
                                spritex = memory.readbyte(0x171F+slot) + memory.readbyte(0x1733+slot)*256
                                spritey = memory.readbyte(0x1715+slot) + memory.readbyte(0x1729+slot)*256
                                extended[#extended+1] = {["x"]=spritex, ["y"]=spritey}
                        end
                end            
               
                return extended
        elseif gameinfo.getromname() == "Super Mario Bros." then
                return {}
        end
end
 
function getInputs()
        getPositions()
       
        sprites = getSprites()
        extended = getExtendedSprites()
       
        local inputs = {}
       
        for dy=-BoxRadius*16,BoxRadius*16,16 do
                for dx=-BoxRadius*16,BoxRadius*16,16 do
                        inputs[#inputs+1] = 0
                       
                        tile = getTile(dx, dy)
                        if tile == 1 and marioY+dy < 0x1B0 then
                                inputs[#inputs] = 1
                        end
                       
                        for i = 1,#sprites do
                                distx = math.abs(sprites[i]["x"] - (marioX+dx))
                                disty = math.abs(sprites[i]["y"] - (marioY+dy))
                                if distx <= 8 and disty <= 8 then
                                        inputs[#inputs] = -1
                                end
                        end
 
                        for i = 1,#extended do
                                distx = math.abs(extended[i]["x"] - (marioX+dx))
                                disty = math.abs(extended[i]["y"] - (marioY+dy))
                                if distx < 8 and disty < 8 then
                                        inputs[#inputs] = -1
                                end
                        end
                end
        end
       
        --mariovx = memory.read_s8(0x7B)
        --mariovy = memory.read_s8(0x7D)
       
        return inputs
end

function randomizeBlocks()
	local number = tonumber(forms.gettext(numberBox))
	if number == nil then
		return
	end
	--shownBlocks = {}
	randomBlocks = {}
	local randomNumber = Inputs/2-math.abs(Inputs/2-number)
	
	for i=1,randomNumber do
		local done = false
		while not done do
			local block = math.random(InputSize)
			if randomBlocks[block] == nil then
				randomBlocks[block] = true
				done = true
			end
		end
	end
	
	if randomNumber == number then
		shownBlocks = randomBlocks
	else
		for i=1,InputSize do
			shownBlocks[i] = true
			if randomBlocks[i] then shownBlocks[i] = false end
		end
	end
end

function drawBlocks()
	local inputs = getInputs()
	local cx, cy = center.x, center.y
	local r = radius
	local d = r+BoxRadius*2*r
	
	gui.drawBox(cx-d,cy-d,cx+d,cy+d, 0xFF000000, 0x00000000)
	
	local i = 1
	for dy=-BoxRadius,BoxRadius,1 do
		for dx=-BoxRadius,BoxRadius,1 do
			local value = inputs[i]
			local x = cx + dx*2*r
			local y = cy + dy*2*r
			local color = 0x00000000
			local border = 0x00000000
			if not shownBlocks[i] then 
				color = 0xFF808080
				border = color
				gui.drawBox(x-r,y-r,x+r-1,y+r-1, border, color)
			else
				local value = inputs[i]
				local x = cx + dx*2*r
				local y = cy + dy*2*r
				local color = 0x00000000
				local border = 0x00000000
				
				if value == 1 then
					color = 0xFFFFFFFF
				elseif value == -1 then
					color = 0xFF000000
				else
					color = 0xFF808090
				end
				border = color
				gui.drawBox(x-r,y-r,x+r-1,y+r-1, border, color)
			end
			i = i + 1
			
		end
	end
	
	local marioColor = 0xFFFF0000
	local x = cx
	local y = cy + 2*r
	gui.drawBox(x-r,y-r,x+r,y+r, marioColor, 0x00000000)
	
end

center = { x = 128, y = 110 }
radius = 8

shownBlocks = {}
for i = 1, InputSize do
	shownBlocks[i] = true
end

form = forms.newform(180, 120, "Are you smarter than MarI/O?")
descLabel = forms.label(form, "Are you smarter than MarI/O?", 5, 5, 160, 20)
numberBox = forms.textbox(form, "#Blocks", 70, 30, nil, 5, 27)
resetButton = forms.button(form, "Randomize Blocks", randomizeBlocks, 80, 25)
enableCheck = forms.checkbox(form, "Active", 10, 50)

while true do
	local x, y = center.x, center.y
	local r = radius
	local d = r+BoxRadius*2*r
	
	if forms.ischecked(enableCheck) then
		gui.drawBox(-1, -1, 256, 244, 0xFF808080, 0xFF808080)
		drawBlocks()
	end
	emu.frameadvance();
end
