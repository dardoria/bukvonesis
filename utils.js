XMLHttpRequest.prototype.sendAsBinary = function(datastr) {
    function byteValue(x) {
        return x.charCodeAt(0) & 0xff;
    }
    var ords = Array.prototype.map.call(datastr, byteValue);
    var ui8a = new Uint8Array(ords);
    console.log(ui8a);
    this.send(ui8a);
}
