-- A substitute for Amphetamine.app
-- All the icons are from https://www.iconfont.cn/search/index?searchType=icon&q=caffeine&page=1&fromCollection=-1

-- TODO:
-- 1. Add a timer to keep awake for a certain time.

local amphetamineIcon = {
   no_amphetamine = hs.image.imageFromPath('assets/amphetamine/no-caffeine.png'):setSize({ w = 20, h = 20 }),
   amphetamine = hs.image.imageFromPath('assets/amphetamine/coffee.png'):setSize({ w = 20, h = 20 }),
}

local amphetamineBar = hs.menubar.new()

local function toggleAmphetamine()
   if hs.caffeinate.toggle('displayIdle') then
      amphetamineBar:setIcon(amphetamineIcon['amphetamine'])
   else
      amphetamineBar:setIcon(amphetamineIcon['no_amphetamine'])
   end
end

amphetamineBar:setIcon(amphetamineIcon['no_amphetamine'])
amphetamineBar:setClickCallback(toggleAmphetamine)
