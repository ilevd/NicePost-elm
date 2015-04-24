var JSONP = function(global){
    function JSONP(uri, callback) {
        function JSONPResponse() {
            try { delete global[src] } catch(e) {
                global[src] = null
            }
            documentElement.removeChild(script);
            callback.apply(this, arguments);
        }
        var src = prefix + id++,
            script = document.createElement("script")
        ;
        global[src] = JSONPResponse;
        documentElement.insertBefore(
            script,
            documentElement.lastChild
        ).src = uri + "&callback=" + src;
    }
    var id = 0,
        prefix = "__JSONP__",
        document = global.document,
        documentElement = document.documentElement;
    return JSONP;
}(this);