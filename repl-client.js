var ReplClient = function (url) {
    var self = this;
    var abort = false;
    var getHttpRequest, log;
    var isTitanium = (typeof Titanium !== "undefined");

    if (isTitanium) {
        log = function (msg) {
            Ti.API.log('log', msg);
        };
        getHttpRequest = function () {
            return Titanium.Network.createHTTPClient();
        };
    } else {
        log = function (msg) {
            console.log(msg);
        };
        getHttpRequest = function () {
            return new XMLHttpRequest();
        };
    }

    var start = function () {
        var req = getHttpRequest();
        req.open('POST', url + '/start', true);
        req.onreadystatechange = function (evt) {
            if (req.readyState === 4) {
                if (req.status === 200) {
                    log("response text is", req.responseText);
                    var response = JSON.parse(req.responseText);
                    self.sid = response.sid;
                    self.rid = Math.floor(Math.random() * 10000000);
                    sendResult(null);
                }
            }
        };
        req.send();
    };

    var stop = function () {
        abort = true;
        var req = getHttpRequest();
        req.open('POST', url + '/' + self.sid + '/stop', true);
        req.send();
    };

    var sendResult = function (result) {
        var req = getHttpRequest();
        req.open('POST', url + '/' + self.sid + '/eval', true);
        req.onreadystatechange = function (evt) {
            if (req.readyState === 4) {
                if (req.status === 200) {
                    var response = JSON.parse(req.responseText);
                    log('To evaluate: ' + response.query);
                    evalRequest(response.query);
                }
            }
        };
        req.send(JSON.stringify({
            rid: self.rid++,
            result: result
        }));
    };

    var evalRequest = function (string) {
        var result, error, props;
        try {
            result = eval(string);
        } catch (e) {
            error = e;
        }
        if (!error) {
            props = examine(result);
        } else {
            props = examine(error);
            props.thrown = true;
        }
        if (!abort) {
            sendResult(JSON.stringify(props));
        }
    };

    var examine = function (x) {
        var props = {};
        props.type = typeof x;
        switch (props.type) {
            case "undefined":
                break;
            case "number":
            case "string":
            case "boolean":
                props.value = x;
                break;
            case "function":
                props.name = x.name;
                if (x.toString) { props.source = x.toString(); }
                break;
            case "object":
                props = examineObject(x);
                break;
            default:
                props.value = x;
        }
        return props;
    };

    var examineObject = function (x) {
        var props = {};
        props.type = "object";
        if (x === null) {
            props.value = x;
        } else if (x instanceof Array) {
            props.constructor = "Array";
            props.value = x;
        } else if (x instanceof Error) {
            props.constructor = "Error";
            props.value = x;
        } else if (x.constructor && x.constructor.name) {
            props.constructor = x.constructor.name;
        }
        return props;
    };


    self.stop = stop;

    start();
};
