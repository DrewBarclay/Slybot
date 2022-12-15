(function(){

console.log("Post.js script");

var system = require('system');
if(system.args.length !== 5 && system.args.length !== 6){
	console.log("Usage: post.js <user> <pass> <forumUrl> <thread> [<msgId>] with stdin as post body")
	phantom.exit(1);
	return null;
}

var stdin = require('fs').open('/dev/stdin', 'r');

settings = {
	user: system.args[1],
	pass: system.args[2],
	forumUrl: system.args[3],
	thread: system.args[4],
	message: stdin.read(),
	msgId: system.args[5]
}


var isEdit = !!settings.msgId;

var page = require('webpage').create();

page.onResourceRequested = function(requestData, request) {
  if (requestData['Content-Type'] == 'application/javascript' || requestData['Content-Type'] == 'text/javascript') {
    console.log('Disabling JavaScript files. Aborting: ' + requestData['url']);
    request.abort();
  }
};
// page.settings.javascriptEnabled = false;

function fail(msg){
		console.log("Problem detected. Exiting post.js");
		msg && console.log("Message: " + msg);
		phantom.exit(1);
}

page.onError = function(msg, trace){
	console.log(msg);
	trace.forEach(function(item) {
	        console.log('  ', item.file, ':', item.line);
	});
        if (msg.indexOf("Can't find variable:") >= 0) {
	     console.log("^ Had error in JS in page but skipping because it don't matter ;)");
        } else {
	    fail();
	}
}

page.onConsoleMessage = function(msg) {
  console.log(msg);
}

/*page.onResourceReceived = function(response) {
    if (response.stage !== "end") return;
    console.log('Response (#' + response.id + ', stage "' + response.stage + '"): ' + response.url);
};

page.onResourceRequested = function(requestData, networkRequest) {
    console.log('Request (#' + requestData.id + '): ' + requestData.url);
};

page.onResourceTimeout = function(request) {
    console.log('Response (#' + request.id + '): ' + JSON.stringify(request));
};*/

page.onResourceError = function(resourceError) {
  // do not print as we do not want to ruin the parsing of the post id at the end...
  // console.log('Unable to load resource (#' + resourceError.id + 'URL:' + resourceError.url + ')');
  //console.log('Error code: ' + resourceError.errorCode + '. Description: ' + resourceError.errorString);
};

page.settings.resourceTimeout = 55000; //55 seconds

console.log('Settings: ' + JSON.stringify(settings));


login(page, function(){ 
	post(page, function(postId){ 
		console.log(postId);
		phantom.exit(0); 
		console.log("... I should be exited?");
	}); 
});

function login(page, callback){
	var loginUrl = settings.forumUrl + "?action=login";
	console.log("Beginning login at: " + loginUrl);
  page.open(loginUrl);
  page.onLoadFinished = function(status) {
    if(status === "success"){
			//page.render("login.png");
			console.log("Login page successfully loaded.");
			// page.settings.javascriptEnabled = true;
			page.evaluate(function(settings){
				console.log("Logging in begun!"); 
				document.querySelector("form[id=frmLogin] input[name=user]").value = settings.user;
				document.querySelector("form[id=frmLogin] input[name=passwrd]").value = settings.pass;
				document.querySelector("form[id=frmLogin]").submit();
        console.log("Login form submit.");
			}, settings);
      page.onLoadStarted = function() {
        console.log("Login submitting...");
      }
			page.onLoadFinished = function(status){ 
				page.onLoadFinished = function(){};
				//page.render("logindone.png");
				console.log("Login presumably complete.");
				callback();
			}	
		}
		else {
			//console.log("Login page failed to load! Status: " + status);
			fail("Login page failed to load, status " + status);
		}
  }
}


function editPost(id, page, callback){
	console.log("Editing post. Opening page...");
	page.open(settings.forumUrl + "?action=post;topic=" + settings.thread + (";msg=" + id), function(status){
		if (status != "success"){ fail(status); }
		console.log("Post page opened.");
		page.evaluate(function(settings){
			document.querySelector("textarea[id=message]").value = settings.message;
			document.querySelector("#postmodify").submit();
		}, settings);
		page.onLoadFinished = function(){ 
			console.log("Post edit complete!");
			page.onLoadFinished = function(){};
			//page.render("postdone.png");
			
			callback(id);
		}
	});	
}

function post(page, callback){
	if(!isEdit){
		console.log("New post creation required.");
		page.open(settings.forumUrl + "?action=post;topic=" + settings.thread, function(status){
			if (status != "success"){ fail(status); }
			console.log("New post page opened.");
			page.onLoadFinished = function(){
				console.log("New post creation finished.");
				page.onLoadFinished = function(){};
				var id = page.evaluate(function(){
					var results = document.querySelectorAll("div.inner[id^=msg_]");
					return results[results.length-1].id.slice(4);
				})
				console.log("New post id: " + id);
				editPost(id, page, callback);
			}
			page.evaluate(function(settings){
				document.querySelector("textarea[id=message]").value = settings.message;
				document.querySelector("#postmodify").submit();
			}, settings);
		});
	}
	else {
		editPost(settings.msgId, page, callback);
	}
}

})();
