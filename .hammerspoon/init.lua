hs.hotkey.alertDuration = 0
hs.hints.showTitleThresh = 0
hs.window.animationDuration = 0

hsreload_keys = hsreload_keys or {{"cmd", "shift", "ctrl"}, "R"}
if string.len(hsreload_keys[2]) > 0 then
    hs.hotkey.bind(hsreload_keys[1], hsreload_keys[2], "Reload configuration!", function() hs.reload() end)
    hs.alert.show("Configuration reloaded!", 0.5)
end


local function titleline(title, signal, length)
   -- This function is for printing titleline.
   if title == nil then
      title = "Missing Title"
   end
   if signal == nil then
      signal = "-"
   else if type(signal) == "table" then
	 if signal.length ~= nil then
	    length = signal.length
	 end
	 signal = signal.signal
      end
   end
   if type(length) == "nil" then -- another way for nil
      length = 40
   end
   local left = (length - #title) // 2
   local right = length - left - #title
   local line = ""
   for i=1,left-1 do
      line = line .. signal
   end
   line = line .. " " .. title .. " "
   for i=1,right-1 do
      line = line .. signal
   end
   return line
end


print(titleline("My configuration"))
require "module.reload"
require "module.window"
require "module.translator"
require "module.wifi"
require "module.screen"
require "module.memory"
require "module.launcher"
require "module.yabai"
