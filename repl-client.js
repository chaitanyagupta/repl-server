var ReplClient = function (url) {
    var abort = false;

    var httpRequest = function (result) {
        var req = new XMLHttpRequest();
        req.open('GET', url + '?result=' + encodeURIComponent(result), true);
        req.onreadystatechange = function (evt) {
            if (req.readyState === 4) {
                if (req.status === 200) {
                    console.log("To evaluate: ", req.responseText);
                    evalRequest(req.responseText);
                }
            }
        };
        req.send(null);
    };

    var evalRequest = function (string) {
        var result, error, props;
        try {
            result = eval(string);
        } catch (e) {
            error = e;
        }
        if (result) {
            props = examine(result);
        } else {
            props = examine(error);
            props.thrown = true;
        }
        if (!abort) {
            httpRequest(JSON.stringify(props));
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
                props.source = x.toString();
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
        if (x instanceof Array) {
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


    this.stop = function () {
        abort = true;
    };

    httpRequest('null');
};
