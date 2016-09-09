require(RSelenium)
# selServ <- startServer()
#
# fprof <- makeFirefoxProfile(list(browser.download.dir = getwd()))



system("java -jar /Library/Frameworks/R.framework/Versions/3.3/Resources/library/RSelenium/bin/selenium-server-standalone.jar -Dwebdriver.chrome.driver=/usr/local/bin/chromedriver", wait=FALSE)
#system("java -jar /Library/Frameworks/R.framework/Versions/3.3/Resources/library/RSelenium/bin/selenium-server-standalone.jar")

pJS <- phantom()
remDr <- remoteDriver(browserName = "phantomjs")



remDr$open()

remDr$navigate("https://eidith.org/PREDICT1/DataViewing/GAINSFileSubmissions.aspx")
userfield = remDr$findElement(value='//*[@id="dnn_ctr866_Login_Login_DNN_txtUsername"]')
pwdfield = remDr$findElement(value='//*[@id="dnn_ctr866_Login_Login_DNN_txtPassword"]')
userfield$sendKeysToElement(list("noamross"))
pwdfield$sendKeysToElement(list("poprox43"))
remDr$findElement(value='//*[@id="dnn_ctr866_Login_Login_DNN_cmdLogin"]')$clickElement()
remDr$findElement(value='//*[@id="dnn_ctr892_SQLGridSelectedView_btnSingleGo"]')$clickElement()

remDr$phantomExecute("
                     var fs=require('fs');
                     page= this;
;

                     fs.makeTree('contents');

                     page.captureContent = ['.*'];

                     page.onResourceReceived = function(response) {
                     page.captureContent = ['.*'];

                     //if(response.stage!='end' || !response.bodySize)return;

                     var logfile = fs.open('console'+response.id+'_'+new Date().getTime()+'.log', 'w');
                     logfile.write(JSON.stringify(response));
                     logfile.close();
                     //console.log('Response (#' + response.id + ', stage ' + response.stage + '): ' + JSON.stringify(response));
                     var matches = response.url.match(/[/]([^/]+)$/);
                     var fname = 'contents/'+matches[1]+new Date().getTime();
                     var file = fs.open(fname, 'w');
                     console.log('Saving '+response.bodySize+' bytes to '+fname);
                     fs.write(fname,response.body);
                     file.close();

                     };

                     page.onResourceRequested = function(requestData, networkRequest) {
                     console.log('Request (#' + requestData.id + '): ' + JSON.stringify(requestData));
                     var file = fs.open('console.log', 'w');
                     file.write(JSON.stringify(requestData));
                     file.close();
                     };

                     page.open(url,function(){
                     phantom.exit();
                     });")

remDr$findElement(value='//*[@id="dnn_ctr892_SQLGridSelectedView_lblExporttoExcel"]')$clickElement()
cat(remDr$getPageSource(header = TRUE, .mapUnicode = FALSE)[[1]], file = 'source.html')
