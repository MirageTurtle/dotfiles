hs.hotkey.alertDuration = 0
hs.hints.showTitleThresh = 0
hs.window.animationDuration = 0

hsreload_keys = hsreload_keys or {{"cmd", "shift", "ctrl"}, "R"}
if string.len(hsreload_keys[2]) > 0 then
    hs.hotkey.bind(hsreload_keys[1], hsreload_keys[2], "Reload configuration!", function() hs.reload() end)
    hs.alert.show("Configuration reloaded!")
end

hs.loadSpoon("ModalMgr")
hs.loadSpoon("WinWin")

-- resizeM modal environment
if spoon.WinWin then
   spoon.ModalMgr:new("resizeM")
   local cmodal = spoon.ModalMgr.modal_list["resizeM"]

   cmodal:bind('', 'escape', 'Deactivate resizeM', function() spoon.ModalMgr:deactivate({"resizeM"}) end)
   cmodal:bind('alt', 'R', 'Deactivate resizeM', function() spoon.ModalMgr:deactivate({"resizeM"}) end)
   cmodal:bind('', 'Q', 'Deactivate resizeM', function() spoon.ModalMgr:deactivate({"resizeM"}) end)
   
   cmodal:bind('', 'H', 'Lefthalf of Screen', function() spoon.WinWin:moveAndResize("halfleft") end)
   cmodal:bind('', 'L', 'Righthalf of Screen', function() spoon.WinWin:moveAndResize("halfright") end)
   cmodal:bind('', 'K', 'Uphalf of Screen', function() spoon.WinWin:moveAndResize("halfup") end)
   cmodal:bind('', 'J', 'Downhalf of Screen', function() spoon.WinWin:moveAndResize("halfdown") end)
   
   cmodal:bind('', 'Y', 'NorthWest Corner', function() spoon.WinWin:moveAndResize("cornerNW") end)
   cmodal:bind('', 'O', 'NorthEast Corner', function() spoon.WinWin:moveAndResize("cornerNE") end)
   cmodal:bind('', 'U', 'SouthWest Corner', function() spoon.WinWin:moveAndResize("cornerSW") end)
   cmodal:bind('', 'I', 'SouthEast Corner', function() spoon.WinWin:moveAndResize("cornerSE") end)
   
   cmodal:bind('', 'F', 'Fullscreen', function() spoon.WinWin:moveAndResize("fullscreen") end)
   cmodal:bind('', 'C', 'Center Window', function() spoon.WinWin:moveAndResize("center") end)
   cmodal:bind('', 'return', 'Maximize', function() spoon.WinWin:moveAndResize("maximize") end)
   
   cmodal:bind('', '=', 'Stretch Outward', function() spoon.WinWin:moveAndResize("expand") end, nil, function() spoon.WinWin:moveAndResize("expand") end)
   cmodal:bind('', '-', 'Shrink Inward', function() spoon.WinWin:moveAndResize("shrink") end, nil, function() spoon.WinWin:moveAndResize("shrink") end)
   -- cmodal:bind('shift', 'H', 'Move Leftward', function() spoon.WinWin:stepMove("left") end, nil, function() spoon.WinWin:stepMove("left") end)
   -- cmodal:bind('shift', 'L', 'Move Rightward', function() spoon.WinWin:stepMove("right") end, nil, function() spoon.WinWin:stepMove("right") end)
   -- cmodal:bind('shift', 'K', 'Move Upward', function() spoon.WinWin:stepMove("up") end, nil, function() spoon.WinWin:stepMove("up") end)
   -- cmodal:bind('shift', 'J', 'Move Downward', function() spoon.WinWin:stepMove("down") end, nil, function() spoon.WinWin:stepMove("down") end)
   -- cmodal:bind('', 'left', 'Move to Left Monitor', function() spoon.WinWin:moveToScreen("left") end)
   -- cmodal:bind('', 'right', 'Move to Right Monitor', function() spoon.WinWin:moveToScreen("right") end)
   -- cmodal:bind('', 'up', 'Move to Above Monitor', function() spoon.WinWin:moveToScreen("up") end)
   -- cmodal:bind('', 'down', 'Move to Below Monitor', function() spoon.WinWin:moveToScreen("down") end)
   cmodal:bind('shift', 'H', 'Move to Left Monitor', function() spoon.WinWin:moveToScreen("left") end)
   cmodal:bind('shift', 'L', 'Move to Right Monitor', function() spoon.WinWin:moveToScreen("right") end)
   cmodal:bind('shift', 'K', 'Move to Above Monitor', function() spoon.WinWin:moveToScreen("up") end)
   cmodal:bind('shift', 'J', 'Move to Below Monitor', function() spoon.WinWin:moveToScreen("down") end)
   -- cmodal:bind('', '`', 'Center Cursor', function() spoon.WinWin:centerCursor() end)

    -- Register resizeM with modal supervisor
   hsresizeM_keys = hsresizeM_keys or {"alt", "R"}
   if string.len(hsresizeM_keys[2]) > 0 then
      spoon.ModalMgr.supervisor:bind(hsresizeM_keys[1], hsresizeM_keys[2], "Enter resizeM Environment", function()
	-- Deactivate some modal environments or not before activating a new one
	spoon.ModalMgr:deactivateAll()
	-- Show an status indicator so we know we're in some modal environment now
	spoon.ModalMgr:activate({"resizeM"}, "#B22222")
      end)
   end
end

	-- Finally we initialize ModalMgr supervisor
spoon.ModalMgr.supervisor:enter()
