-- Based on https://github.com/sugood/hammerspoon/
-- Translator

transHistory = "~/.hammerspoon/data/transHistory.json"
transConfig = "~/.hammerspoon/data/transConfig.json"

local config = hs.json.read(transConfig)

-- global variables
local baiduUrl = "https://fanyi-api.baidu.com/api/trans/vip/translate"
local baiduWeb = "https://fanyi.baidu.com/"

-- control variables
local isFirst = true
local mChooser

-- util functions
function decodeURI(s)
    s = string.gsub(s, "([^%w%.%- ])", function(c) return string.format("%%%02X", string.byte(c)) end)
    return string.gsub(s, " ", "+")
end

function newWebview()
    local cscreen = hs.screen.mainScreen()
    local cres = cscreen:fullFrame()
    sheetView = hs.webview.newBrowser({
        x = cres.x+cres.w*0.15/2,
        y = cres.y+cres.h*0.25/2,
        w = cres.w*0.85,
        h = cres.h*0.75
    })
    sheetView:windowTitle("CheatSheets")
    sheetView:windowStyle("utility")
    sheetView:titleVisibility("hidden")
    sheetView:allowGestures(true)
    sheetView:allowNewWindows(false)
    sheetView:allowTextEntry(true)
    sheetView:closeOnEscape(true)
    sheetView:bringToFront(false)
    sheetView:darkMode(true)
    sheetView:reload(false)
    sheetView:level(hs.drawing.windowLevels.mainMenu)
end

-- copy for paste
local function completionFn(result)
    if result then
        hs.pasteboard.setContents(result.text)
        --hs.eventtap.keyStroke({ "cmd" }, "V")
        print("result: " .. result.text)
    end
end

-- input listener
local function queryChangedCallbackFn(result)
    print("data: " .. result)
    if result and #result > 0 then
        if config.engine == 'baidu' then
            getBaiduApi(result)
        else
	    print("Engine only supports Baidu.")
            -- getYoudaoApi(result)
        end
    else
        if(isFirst) then
            isFirst = false
        else
            search = {}
            mChooser:choices(search)
            hs.json.write(search, transHistory, true, true)
        end
    end
end

-- baidu API
function getBaiduApi(data)
    headers = {ContentType = "application/x-www-form-urlencoded;charset=utf-8"}
    salt = tostring(hs.timer.secondsSinceEpoch()*1000)
    curtime = tostring(os.time())
    params = "q=" .. data .. "&from=auto&to=auto&appid=" .. config.baiduAppid .. "&salt=" .. salt .. "&sign=" .. hs.hash.MD5(config.baiduAppid .. data .. salt .. config.baiduAppSecret)
    print('params:' .. params)
    hs.http.doAsyncRequest(baiduUrl, "POST", params, headers, function(code, body, htable)
	if code ~= 200 then
	   print("Request Baidu API error: " .. code)
	   return
	else
	   search = {}
	   result = hs.json.decode(body)
	   print("result: " .. hs.inspect(result))
	   if result["trans_result"] then
	      item = {}
	      item.text = result["trans_result"][1].dst
	      item.subText = data
	      item.webUrl = baiduWeb .. result["from"] .. "/" .. result["to"] .. "/" .. decodeURI(data)
	      table.insert(search, 1, item)
	   end
	   mChooser:choices(search)
	   hs.json.write(search, transHistory, true, true)
	end
    end)
end

function init()
   newWebview()
   hs.hotkey.bind({"alt"}, "space", function()
       ifFirst = true
       res = hs.json.read(transHistory)
       if res == nil then
	  res = {}
       end
       local num = hs.pasteboard.changeCount()
       hs.eventtap.keyStroke({"cmd"}, "C")
       mChooser = hs.chooser.new(completionFn)
	  :choices(res)
	  :queryChangedCallback(queryChangedCallbackFn)
	  :searchSubText(true)
	  :show()
       local numNew = hs.pasteboard.changeCount()
       if numNew > num then
	  hs.eventtap.keyStroke({"cmd"}, "V")
       end
   end)
end
          
init()
