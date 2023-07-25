-- Based on hammerspoon-config
-- Window management

-- Define window maximize toggler
local frameCache = {}
local logger = hs.logger.new("window")

function winResize(method)
   --[[
      method: one of "left", "right", "up", "down", "max",
      "hthird-0", "hthird-1", "hthird-2",
      "vthird-0", "vthird-1", "vthird-2"
   ]]
   local win = hs.window.focusedWindow()
   local newWin

   local method_table = {
      ["left"] = hs.layout.left50,
      ["right"] = hs.layout.right50,
      ["up"] = {0, 0, 1, 0.5},
      ["down"] = {0, 0.5, 1, 0.5},
      ["max"] = hs.layout.maximized,
      ["hthird-0"] = {0, 0, 1/3, 1},
      ["hthird-1"] = {1/3, 0, 1/3, 1},
      ["hthird-2"] = {2/3, 0, 1/3, 1},
      ["vthird-0"] = {0, 0, 1, 1/3},
      ["vthird-1"] = {0, 1/3, 1, 1/3},
      ["vthird-2"] = {0, 2/3, 1, 1/3},
   }
   newWin = method_table[method]

   win:move(newWin)
end   

hs.hotkey.bind({"ctrl", "alt"}, "left", hs.fnutils.partial(winResize, "left"))
hs.hotkey.bind({"ctrl", "alt"}, "right", hs.fnutils.partial(winResize, "right"))
hs.hotkey.bind({"ctrl", "alt"}, "up", hs.fnutils.partial(winResize, "up"))
hs.hotkey.bind({"ctrl", "alt"}, "down", hs.fnutils.partial(winResize, "down"))
hs.hotkey.bind({"ctrl", "alt"}, "return", hs.fnutils.partial(winResize, "max"))
