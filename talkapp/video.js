/* initialize the video */

var video = 
    (function () {
         swfobject["video_log"] = function (str) {
             console.log("video " + str);
         };
        
         var display = function () {
             swfobject.video_log("doing video display");

             // Work out the height of the video.
             $("#videocall").removeClass("hidden");

             $("#end-call").on(
                 "click",
                 function (evt) {
                     $("#videocall").addClass("hidden");
                 }
             );

             var height = $("#video")[0].clientHeight - 1;
             var width = $("#video")[0].clientWidth - 1;
             
             swfobject.video_log("height = " + height);
             
             var flashvars = {
                 size: "2",
                 height: "" + height,
                 width: "" + width,
                 wowza: "some url",
                 log: "swfobject.video_log",
                 flash_inited: "video.flash_inited",
             };
             
             var params = {
                 allowScriptAccess: "always",
                 wmode: "opaque",
                 bgcolor: "#FFFFFF"
             };
             
             swfobject.embedSWF(
                 "/-/vidclient.swf",
                 "video",
                 width + "px",
                 height + "px",
                 "10.0.0",
                 "",
                 flashvars, 
                 params
             );
         };

         var inited = function () {
             swfobject.video_log("inited called");
             var camlist = swfobject.getObjectById('video').cameras();
             swfobject.getObjectById('video').camera_select(camlist);
         };
         
         return {
             display: display,
             flash_inited: inited
         };
     }
    )();

/* end */
