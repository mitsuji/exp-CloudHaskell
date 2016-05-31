window.addEventListener( 'load', function(event){

    var clientName = window.prompt('May I ask your name?','[anonymous]');
    
//    var ws = new WebSocket('ws://' + window.location.host + '/client?' + encodeURIComponent(clientName));
    var ws = new WebSocket('ws://' + window.location.host + '/client?' + clientName);
	
    ws.onopen = function() {
    };
	
    ws.onclose = function(event) {
    };
	
    ws.onmessage = function(event) {
	console.log('onmessage: ' + event.data);
	var logList = document.getElementById('msg-log');
	var logLine = document.createElement('div');
	logLine.innerHTML = event.data;
	logList.insertBefore(logLine,logList.firstChild);
    };
    
    document.getElementById('send-button').addEventListener('click', function(event){
	var msg = document.getElementById('msg-text').value;
	ws.send(msg);
    });

    
}, false );
