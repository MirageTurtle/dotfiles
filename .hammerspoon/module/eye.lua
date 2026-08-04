-- Eye Break Reminder

local eyeBreak = hs.menubar.new()
if not eyeBreak then
  hs.alert.show("Could not create menubar item for eye break reminder.")
  return
end

eyeBreak:setTitle("👁")

local timer = nil
local countdownTimer = nil
local breakCanvases = {}
local dismissHotkey = nil

local function stopCountdown()
   if countdownTimer then
      countdownTimer:stop()
      countdownTimer = nil
   end
end

local function updateCountdown()
   if not timer or not timer:running() then
      return
   end

   local secondsLeft = math.max(0, math.ceil(timer:nextTrigger()))
   local hours = math.floor(secondsLeft / 3600)
   local minutes = math.floor(secondsLeft % 3600 / 60)
   local seconds = secondsLeft % 60

   if hours > 0 then
      eyeBreak:setTitle(string.format("👁 %d:%02d:%02d", hours, minutes, seconds))
   else
      eyeBreak:setTitle(string.format("👁 %02d:%02d", minutes, seconds))
   end
end

local function dismissEyeBreak()
   if dismissHotkey then
      dismissHotkey:delete()
      dismissHotkey = nil
   end

   for _, canvas in ipairs(breakCanvases) do
      canvas:delete()
   end
   breakCanvases = {}
end

local function showEyeBreak()
   dismissEyeBreak()

   for _, screen in ipairs(hs.screen.allScreens()) do
      local canvas = hs.canvas.new(screen:fullFrame())
      canvas[1] = {
         type = "rectangle",
         action = "fill",
         fillColor = { white = 1, alpha = 1 },
         frame = { x = 0, y = 0, w = "100%", h = "100%" },
         trackMouseUp = true,
      }
      canvas[2] = {
         type = "text",
         text = "Time to take a break for your eyes!",
         textAlignment = "center",
         textColor = { white = 0.15, alpha = 1 },
         textSize = 42,
         frame = { x = 0, y = "45%", w = "100%", h = "10%" },
      }
      canvas:level(hs.canvas.windowLevels.screenSaver)
      canvas:behavior({ "canJoinAllSpaces", "stationary", "fullScreenAuxiliary" })
      canvas:clickActivating(false)
      canvas:mouseCallback(function(_, message)
         if message == "mouseUp" then
            dismissEyeBreak()
         end
      end)
      canvas:show()
      table.insert(breakCanvases, canvas)
   end

   dismissHotkey = hs.hotkey.bind({}, "escape", dismissEyeBreak)
end

local function enableEyeBreakWithTimeout(timeout)
   -- timeout in minutes
   -- Cancel any existing timer
   if timer then
      timer:stop()
      timer = nil
   end
   stopCountdown()
   timer = hs.timer.doAfter(timeout * 60, function()
      timer = nil
      stopCountdown()
      eyeBreak:setTitle("👁")
      showEyeBreak()
   end)
   updateCountdown()
   countdownTimer = hs.timer.doEvery(1, updateCountdown)
   hs.alert.show("Eye break reminder set for " .. timeout .. " minutes.")
end

-- set click callback for the menubar item
eyeBreak:setMenu({
      { title = 'Enable for 15 minutes', fn = function() enableEyeBreakWithTimeout(15) end },
      { title = 'Enable for 30 minutes', fn = function() enableEyeBreakWithTimeout(30) end },
      { title = 'Enable for 1 hour', fn = function() enableEyeBreakWithTimeout(60) end },
      { title = '-' },
      { title = 'Disable', fn = function()
	 if timer then
	    timer:stop()
	    timer = nil
	 end
	 stopCountdown()
	 dismissEyeBreak()
	 eyeBreak:setTitle("👁")
	 hs.alert.show("Eye break reminder disabled.")
      end }
})
