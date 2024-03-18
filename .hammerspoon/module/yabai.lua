-- Some keybindings for yabai
-- The yabai modifier is cmd+option+ctrl+shift, which I use Karabiner-Elements to map to right_option
-- I try to write a key-map using hammerspoon but failed, add it to my todo list maybe
-- local yabai_modifier = {'cmd', 'alt', 'ctrl', 'shift'}
local function yabai (commands)
   for _, command in ipairs(commands) do
      print('yabai -m ' .. command)
      hs.execute('yabai -m ' .. command)
   end
end

local function alt(key, commands)
   hs.hotkey.bind({'alt'}, key, function ()
	 yabai(commands)
   end)
end

local function alt_shift(key, commands)
   hs.hotkey.bind({'alt', 'shift'}, key, function ()
	 yabai(commands)
   end)
end

local alt_keybindings = {
   -- letters
   {key="m", commands={"space --toggle mission-control"}},
   {key="r", commands={"space --rotate 90"}},
   {key="t", commands={"window --toggle float", "window --grid 4:4:1:1:2:2"}},
   -- special characters
   {key="tab", commands={"space --focus recent"}},
   {key="'", commands={"space --layout bsp"}},
}

local alt_shift_keybinds = {
   {key="'", commands={"space --layout stack"}},
}

-- numbers
for i=1,9 do
   local key = tostring(i)
   table.insert(alt_keybindings, {key=key, commands={"space --focus " .. key}})
   table.insert(alt_shift_keybinds, {key=key, commands={"window --space " .. key, "space --focus " .. key}})
end
-- hjkl
local hjkl = {h="west", j="south", k="north", l="east"}
for key, direction in pairs(hjkl) do
   table.insert(alt_keybindings, {key=key, commands={"window --focus " .. direction}})
   table.insert(alt_shift_keybinds, {key=key, commands={"window --swap " .. direction}})
end

-- map the keybindings
for _, keybinding in ipairs(alt_keybindings) do
	alt(keybinding.key, keybinding.commands)
end
for _, keybinding in ipairs(alt_shift_keybinds) do
	alt_shift(keybinding.key, keybinding.commands)
end
