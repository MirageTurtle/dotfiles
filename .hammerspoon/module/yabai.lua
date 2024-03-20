-- Some keybindings for yabai
-- Deprecaed:
-- The yabai modifier is cmd+option+ctrl+shift, which I use Karabiner-Elements to map to right_option
-- I try to write a key-map using hammerspoon but failed, add it to my todo list maybe
-- local yabai_modifier = {'cmd', 'alt', 'ctrl', 'shift'}

local function yabai (commands)
   for _, command in ipairs(commands) do
      print('yabai -m ' .. command)
      hs.execute('/usr/local/bin/yabai -m ' .. command)
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

local function toggle_layout()
   -- local layout_list = {"bsp", "float", "stack"}
   local layout_list = {"bsp", "float"}
   local current_layout = hs.execute("/usr/local/bin/yabai -m query --spaces --space | /usr/local/bin/jq -r '.type'")
   current_layout = string.gsub(current_layout, "\n", "")
   local next_layout = layout_list[1]
   for i, layout in ipairs(layout_list) do
      -- layout == current_layout will always be false
      -- print type of layout and current_layout to debug
      if layout == current_layout then
	 next_layout = layout_list[(i % #layout_list) + 1]
	 break
      end
   end
   hs.notify.new({title="Yabai", informativeText="Switching layout to " .. next_layout}):send()
   yabai({"space --layout " .. next_layout})
end

local alt_keybindings = {
   -- letters
   {key="m", commands={"space --toggle mission-control"}},
   {key="r", commands={"space --rotate 90"}},
   -- {key="t", commands={"window --toggle float", "window --grid 4:4:1:1:2:2"}},
   {key = "t", commands={"window --toggle zoom-fullscreen"}},
   -- special characters
   {key="tab", commands={"space --focus recent"}},
   -- use ' to toggle layout between bsp, float and stack
   -- {key="'", commands={"space --layout bsp"}},
}

local alt_shift_keybinds = {
   -- {key="'", commands={"space --layout stack"}},
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
hs.hotkey.bind({'alt'}, "'", function ()
      toggle_layout()
end)
for _, keybinding in ipairs(alt_shift_keybinds) do
	alt_shift(keybinding.key, keybinding.commands)
end
