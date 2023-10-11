builtin = nil
aoc = nil

function update_screens()
    screens = hs.screen.allScreens()
    builtin = nil
    aoc = nil
    for _, screen in pairs(screens)
    do
        if screen:name() == "Built-in Display" then
            builtin = screen
        elseif screen:name() == "U27G3X" then
            aoc = screen
        end
    end
    if aoc == nil then
        hs.notify.new({title="Hammerspoon", informativeText="No AOC screen found."}):send()
    end
end

function double_screens(primary, external, position)
    -- position means external screen's position
    -- set primary
    primary:mirrorStop(true)
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
end

-- Function for single screen
-- primary screen: U27G3X
-- mirror screen: Built-in Display
-- lightness of mirror screen: 0
function only_external(builtin, external)
    external:setPrimary()
    builtin:mirrorOf(external)
    builtin:setBrightness(0)
end

screenWatcher = hs.screen.watcher.new(update_screens)

hs.hotkey.bind({"alt", "ctrl"}, "0", function()
    update_screens()
    double_screens(builtin, aoc, "up")
end)
hs.hotkey.bind({"alt", "ctrl"}, "1", function()
    update_screens()
    only_external(builtin, aoc)
end)