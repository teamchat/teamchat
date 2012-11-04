/* initialize the video */

var video_server;
var video_me;
var video_them;
var video = 
    (function () {
         swfobject["video_log"] = function (str) {
             console.log("video " + str);
         };

         /** Take a server and person A and person B.
          */
         var display = function (server, a, b) {
             video_server = server;
             video_me = a;
             video_them = b;
             swfobject.video_log("doing video display " 
                                 + video_server 
                                 + " " + a 
                                 + " " + b);

             // Work out the height of the video.
             $("#videocall").removeClass("hidden");

             var height = $("#video")[0].clientHeight - 1;
             var width = $("#video")[0].clientWidth - 1;
             
             swfobject.video_log("height = " + height);
             
             var flashvars = {
                 size: "2",
                 height: "" + height,
                 width: "" + width,
                 wowza: "rtmp://localhost",
                 log: "swfobject.video_log",
                 flash_inited: "video.flash_inited",
                 flash_connected: "video.flash_connected",
                 flash_published: "video.flash_published",
                 flash_subscribed: "video.flash_subscribed"
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

             // Where are we gonna pull these from?
             try {
                 swfobject.getObjectById("video").connect(
                     video_server, 
                     "/vidclient"
                 );
             } catch (x) {
                 swfobject.video_log("failed to connect: " + x);
             }
         };

         var connected = function () {
             swfobject.video_log("flash connected!");
             swfobject.getObjectById("video").send_me(video_me);
         };

         var published = function () {
             swfobject.video_log("flash published!");
             swfobject.getObjectById("video").get_them(video_them);
         };

         var subscribed = function () {
             swfobject.video_log("flash published!");
         };

         
         return {
             display: display,
             flash_inited: inited,
             flash_connected: connected,
             flash_published: published
         };
     }
    )();

/* end */
