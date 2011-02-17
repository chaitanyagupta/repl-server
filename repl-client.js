var _, __, ___;
var _$, _$$, _$$$;

var ReplClient = function (url) {
    var self = this;
    var abort = false;
    var getHttpRequest, log, warn;
    var isTitanium = (typeof Titanium !== "undefined");

    if (isTitanium) {
        log = function (msg) {
            Ti.API.log('LOG', msg);
        };
        warn = function (msg) {
            Ti.API.warn(msg);
        };
        getHttpRequest = function () {
            return Titanium.Network.createHTTPClient({
                timeout: 130*1000
            });
        };
    } else {
        log = function (msg) {
            console.log(msg);
        };
        warn = function (msg) {
            console.warn(msg);
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
                    log('response text is: ' + req.responseText);
                    var response = JSON.parse(req.responseText);
                    self.sid = response.sid;
                    self.rid = Math.floor(Math.random() * 10000000);
                    sendResult(null);
                }
            }
        };
        req.send(JSON.stringify({
            version: ReplClient.version
        }));
    };

    var stop = function () {
        abort = true;
        var req = getHttpRequest();
        req.open('POST', url + '/' + self.sid + '/stop', true);
        req.send();
    };

    var sendResult = function (result) {
        var req = getHttpRequest();
        var done = false;
        req.open('POST', url + '/' + self.sid + '/eval', true);
        req.onreadystatechange = function (evt) {
            if (req.readyState === 4) {
                if (req.status === 200) {
                    if (!done) {
                        var response = JSON.parse(req.responseText);
                        if (typeof response.query !== "undefined") {
                            log('To evaluate: ' + response.query);
                            evalRequest(response.query);
                        } else {
                            log('No query received');
                            sendResult(null);
                        }
                        done = true;
                    } else {
                        warn('Already done with this request: ' + result);
                    }
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
        var last = _;
        try {
            result = eval(string);
            _ = result;
        } catch (e) {
            error = e;
            _ = undefined;
        }
        _$$$ = _$$;
        _$$ = _$;
        _$ = string;
        ___ = __;
        __ = last;
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
        if (typeof x === "undefined") {
            props.type = "Undefined";
        } else if (x === null) {
            props.type = "Null";
        } else if (typeof x === "number") {
            props.type = "Number";
            if (!isNaN(x) && isFinite(x)) {
                props.value = x;
            } else {
                props.value = x.toString();
            }
        } else if (typeof x === "string") {
            props.type = "String";
            props.value = x;
        } else if (typeof x === "boolean") {
            props.type = "Boolean";
            props.value = x;
        } else if (x instanceof RegExp) {
            props.type = "RegExp";
            props.value = x.toString();
        } else if (x instanceof Array) {
            props.type = "Array";
            props.value = x;
        } else if (x instanceof Error) {
            props.type = "Error";
            props.constructor = x.constructor && x.constructor.name;
            props.error = x.toString();
        } else if (typeof x === "function") {
            props.type = "Function";
            props.name = x.name;
            if (x.toString) { props.source = x.toString(); }
        } else if (typeof x === "object") {
            props.type = "Object";
            if (x.constructor && x.constructor.name) {
                props.constructor = x.constructor.name;
            }
        } else {
            props.type = "Unknown";
        }
        return props;
    };

    self.stop = stop;

    start();
};

ReplClient.version = "0.1.1";
