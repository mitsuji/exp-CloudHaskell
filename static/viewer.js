window.addEventListener( 'load', function(event){


    var ws = new WebSocket('ws://' + window.location.host + '/viewer');
	
    ws.onopen = function() {
    };
	
    ws.onclose = function(event) {
    };
	
    ws.onmessage = function(event) {
	console.log('onmessage: ' + event.data);
    };
    
    
}, false );
