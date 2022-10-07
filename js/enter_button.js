function EnterClick(ns){
$(document).keyup(function(event) {
    if ($("#" + ns + "txt").is(":focus") && (event.key == "Enter")) {
        $("#"+ ns + "action").click();
    }
});
$(document).keyup(function(event) {
    if ($("#" + ns + "gtxt").is(":focus") && (event.key == "Enter")) {
        $("#"+ ns + "action2").click();
    }
});
}