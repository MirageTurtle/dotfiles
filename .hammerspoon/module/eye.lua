-- Eye Break Reminder

local eyeBreak = hs.menubar.new()
if not eyeBreak then
  hs.alert.show("Could not create menubar item for eye break reminder.")
  return
end

eyeBreak:setTitle("👁")

local timer = nil

local function enableEyeBreakWithTimeout(timeout)
   -- timeout in minutes
   -- Cancel any existing timer
   if timer then
      timer:stop()
      timer = nil
   end
   timer = hs.timer.doAfter(timeout * 60, function()
      eyeBreak:setTitle("👁")
      hs.alert.show("Time to take a break for your eyes!")
   end)
   eyeBreak:setTitle("⏳")
   hs.alert.show("Eye break reminder set for " .. timeout .. " minutes.")
end

-- set click callback for the menubar item
eyeBreak:setMenu({
      { title = 'Enable for test', fn = function() enableEyeBreakWithTimeout(0.1) end },
      { title = 'Enable for 15 minutes', fn = function() enableEyeBreakWithTimeout(15) end },
      { title = 'Enable for 30 minutes', fn = function() enableEyeBreakWithTimeout(30) end },
      { title = 'Enable for 1 hour', fn = function() enableEyeBreakWithTimeout(60) end },
      { title = '-' },
      { title = 'Disable', fn = function()
	 if timer then
	    timer:stop()
	    timer = nil
	 end
	 eyeBreak:setTitle("👁")
	 hs.alert.show("Eye break reminder disabled.")
      end }
})
