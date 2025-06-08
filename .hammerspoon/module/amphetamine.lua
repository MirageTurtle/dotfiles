-- A substitute for Amphetamine.app
-- All the icons are from https://www.iconfont.cn/search/index?searchType=icon&q=caffeine&page=1&fromCollection=-1

local configDir = hs.configdir

local amphetamineIcon = {
   no_amphetamine = hs.image.imageFromPath(configDir .. '/assets/amphetamine/no-caffeine.png'):setSize({ w = 20, h = 20 }),
   amphetamine = hs.image.imageFromPath(configDir .. '/assets/amphetamine/coffee.png'):setSize({ w = 20, h = 20 }),
}

local amphetamineBar = hs.menubar.new()

local function toggleAmphetamine()
   if hs.caffeinate.toggle('displayIdle') then
      amphetamineBar:setIcon(amphetamineIcon['amphetamine'])
   else
      amphetamineBar:setIcon(amphetamineIcon['no_amphetamine'])
   end
end

local function enableAmphetamineWithTimeout(timeout)
   hs.caffeinate.set('displayIdle', true)
   amphetamineBar:setIcon(amphetamineIcon['amphetamine'])
   hs.timer.doAfter(timeout, function()
      hs.caffeinate.set('displayIdle', false)
      amphetamineBar:setIcon(amphetamineIcon['no_amphetamine'])
   end)
end

amphetamineBar:setIcon(amphetamineIcon['no_amphetamine'])
-- amphetamineBar:setClickCallback(toggleAmphetamine)
amphetamineBar:setMenu({
   { title = 'Toggle', fn = function() toggleAmphetamine() end },
   { title = 'Enable for 5 minutes', fn = function() enableAmphetamineWithTimeout(300) end },
   { title = 'Enable for 15 minutes', fn = function() enableAmphetamineWithTimeout(900) end },
   { title = 'Enable for 30 minutes', fn = function() enableAmphetamineWithTimeout(1800) end },
   { title = 'Enable for 1 hour', fn = function() enableAmphetamineWithTimeout(3600) end },
   { title = 'Enable for 2 hours', fn = function() enableAmphetamineWithTimeout(7200) end },
   { title = 'Enable for 4 hours', fn = function() enableAmphetamineWithTimeout(14400) end },
   { title = 'Enable for 8 hours', fn = function() enableAmphetamineWithTimeout(28800) end },
   { title = 'Enable for 12 hours', fn = function() enableAmphetamineWithTimeout(43200) end },
   { title = 'Enable for 24 hours', fn = function() enableAmphetamineWithTimeout(86400) end },
   { title = '-' },
   -- { title = 'Disable', fn = function() hs.caffeinate.set('displayIdle', false) amphetamineBar:setIcon(amphetamineIcon['no_amphetamine']) end },
})
