builtin = nil
external = nil

function update_screens()
    screens = hs.screen.allScreens()
    builtin = nil
    external = nil
    for _, screen in pairs(screens)
    do
        brightness = screen:getBrightness()
        if brightness == nil then
            brightness = "nil"
        end
        print(screen:name() .. ": " .. brightness)
        if screen:name() == "Built-in Display" or screen:name() == "Built-in Retina Display" then
            builtin = screen
        -- elseif screen:name() == "U27G3X" then
	else
            external = screen
        end
    end
    if external == nil then
        hs.notify.new({title="Hammerspoon", informativeText="No external screen found."}):send()
        -- if hs.execute("defaults read com.apple.dock autohide") == "0\n" then
        --     hs.execute("defaults write com.apple.dock autohide -bool true")
	--     hs.execute("defaults write com.apple.dock magnification -bool true")
        --     hs.execute("killall Dock")
        -- end
    end
end

function double_screens(primary, external, position)
    -- position means external screen's position
    if primary == nil or external == nil then
        print("Target screen(s) miss.")
        return
    end
    -- set primary
    primary:setPrimary()
    -- compute position
    if position == "up" then
        x = (primary:currentMode().w - external:currentMode().w) // 2
        y = 0 - external:currentMode().h
    elseif position == "down" then
        x = (primary:currentMode().w - external:currentMode().w) // 2
        y = primary:currentMode().h
    end
    -- set external
    external:setOrigin(x, y)
    -- set brightness
    for _, screen in pairs(hs.screen.allScreens())
    do
        brightness = screen:getBrightness()
        if brightness ~= nil and brightness < 1e-5 then
            screen:setBrightness(0.5)
        end
    end
    if hs.execute("defaults read com.apple.dock autohide") == "0\n" then
        hs.execute("defaults write com.apple.dock autohide -bool true")
	hs.execute("defaults write com.apple.dock magnification -bool true")
        hs.execute("killall Dock")
    end
end

-- Function for single screen
-- primary screen: external
-- mirror screen: Built-in Display
-- Brightness of mirror screen: 0
function only_external(builtin, external)
    if builtin == nil or external == nil then
        print("Target screen(s) miss.")
        return
    end
    external:setPrimary()
    builtin:mirrorOf(external)
    builtin:setBrightness(0)
    if hs.execute("defaults read com.apple.dock autohide") == "1\n" then
        hs.execute("defaults write com.apple.dock autohide -bool false")
	hs.execute("defaults write com.apple.dock magnification -bool false")
        hs.execute("killall Dock")
    end
end

-- Function for presentation
function mirror_builtin(builtin, external)
    if builtin == nil or external == nil then
        print("Target screen(s) miss.")
        return
    end
    builtin:setPrimary()
    external:mirrorOf(builtin)
    -- set brightness
    for _, screen in pairs(hs.screen.allScreens())
    do
        brightness = screen:getBrightness()
        if brightness ~= nil and brightness < 1e-5 then
        screen:setBrightness(0.5)
        end
    end
    if hs.execute("defaults read com.apple.dock autohide") == "0\n" then
        hs.execute("defaults write com.apple.dock autohide -bool true")
	hs.execute("defaults write com.apple.dock magnification -bool true")
        hs.execute("killall Dock")
    end
end

-- Function to toggle the dock autohide and magnification
function toggle_dock()
    if hs.execute("defaults read com.apple.dock autohide") == "0\n" then
	hs.execute("defaults write com.apple.dock autohide -bool true")
	hs.execute("defaults write com.apple.dock magnification -bool true")
	hs.execute("killall Dock")
    else
	hs.execute("defaults write com.apple.dock autohide -bool false")
	hs.execute("defaults write com.apple.dock magnification -bool false")
	hs.execute("killall Dock")
    end
end

screenWatcher = hs.screen.watcher.new(update_screens)
screenWatcher:start()

hs.hotkey.bind({"alt", "ctrl"}, "0", function()
    main = hs.screen.mainScreen()
    main:mirrorStop()
    update_screens()
    double_screens(builtin, external, "up")
end)
hs.hotkey.bind({"alt", "ctrl"}, "1", function()
    update_screens()
    only_external(builtin, external)
end)
hs.hotkey.bind({"alt", "ctrl"}, "2", function()
    main = hs.screen.mainScreen()
    main:mirrorStop()
    update_screens()
    mirror_builtin(builtin, external)
end)

hs.hotkey.bind({"alt", "ctrl"}, "d", function()
    toggle_dock()
end)
