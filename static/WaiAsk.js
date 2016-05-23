window.addEventListener('load',function(event){

    document.getElementById('send-button').addEventListener('click', function(event){

	var msg = document.getElementById('msg-text').value;

	var req = new XMLHttpRequest();
	req.onreadystatechange = function(){
	    if (this.readyState == 4){
		if (this.status == 200){
		    document.getElementById('result').innerHTML = this.responseText;
		}else{
		    console.log("response status was not 200 ...");
		}
	    }
	}
	req.open('POST', "/ask", true);
	req.setRequestHeader('Content-Type','text/plain');
	req.send(msg);

    }, false);
    
},false);

