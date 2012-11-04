package {
    import flash.display.LoaderInfo;
    import flash.display.StageAlign;
    import flash.display.StageScaleMode;
    import flash.display.Bitmap;
    import flash.display.BitmapData;
    import flash.display.Shape;
    import flash.geom.Rectangle;
    import flash.geom.Matrix;
    import flash.filters.ColorMatrixFilter;
    import flash.filters.BlurFilter;
    import flash.events.Event;
    import flash.events.ActivityEvent;
    import flash.events.HTTPStatusEvent
    import flash.events.AsyncErrorEvent;;
    import flash.events.HTTPStatusEvent;
    import flash.events.NetStatusEvent;
    import flash.events.IOErrorEvent;
    import flash.events.ProgressEvent;
    import flash.events.SecurityErrorEvent;
    import flash.events.TimerEvent;
    import flash.net.NetStream;
    import flash.net.NetConnection;
    import flash.net.URLVariables;
    import flash.net.URLLoader;
    import flash.net.URLRequest;
    import flash.utils.Timer;
    import flash.utils.ByteArray;    
    import flash.media.Camera;
    import flash.media.Microphone;
    import flash.media.Video;
    import flash.media.SoundTransform;
    import flash.media.SoundMixer;
    import flash.external.ExternalInterface;
    import flash.system.Security;
    import flash.system.SecurityPanel;
    import flash.text.Font;
    import mx.controls.Text;
    import mx.containers.Canvas;
    import mx.core.Application;    
    import mx.core.UIComponent;
    import mx.core.Container;
    import mx.controls.VideoDisplay;
    import mx.events.FlexEvent;
    import com.adobe.images.JPGEncoder;

    public class VidClient extends Application {
        public var mxml_displayBox:Container;
        private var video:Video;
        private var camera:Camera;
        private var microphone:Microphone;
        private var nc:NetConnection;
        private var myStream:NetStream = null;
        private var canvas:Array;
        private var videoPanels:Array;
        private var netStreams:Array;
        private var videos:Array;
        private var myId:String;
        private var muted:Boolean = false;
        private var debugDisplay:Boolean = false;

        // The JS methods this video client calls.
        private var javascript_method_flash_disconnected:String;
        private var javascript_method_flash_connected:String;
        private var javascript_method_flash_novidstream:String;
        private var javascript_method_flash_failed:String;
        private var javascript_method_flash_vidstream:String;
        private var javascript_method_flash_notify:String;
        private var javascript_method_flash_cameraon:String;
        private var javascript_method_flash_initialized:String;
        private var javascript_method_flash_published:String;
        private var javascript_method_flash_subscribed:String;
        private var javascript_method_set_hash_id:String;
        private var javascript_method_log:String;

        public function VidClient() {
            addEventListener(FlexEvent.APPLICATION_COMPLETE, mainInit);
        }

        public function mainInit(event:FlexEvent):void {
            // class constructor
            flash.system.Security.allowDomain("*");
            
            var flashVars:Object = LoaderInfo(this.root.loaderInfo).parameters;
            var size:int = 2;
            var height:int = flashVars["height"];
            var width:int = flashVars["width"] / 2;
            javascript_method_log = flashVars["log"];

            debug("height,width = " + height + "," + width);
            
            canvas = new Array(size);
            videoPanels = new Array(size);
            netStreams = new Array(size);
            videos = new Array(size);
            
            stage.align = "TL";
            stage.scaleMode = "noScale";

            for (var i:int = 0; i < size; i++) {
                videoPanels[i] = new VideoDisplay();
                videoPanels[i].volume = 1.0;
                videoPanels[i].width = width;
                videoPanels[i].height = height;

                canvas[i] = new Canvas();
                canvas[i].width = videoPanels[i].width;
                canvas[i].height = videoPanels[i].height;
                canvas[i].addChild(videoPanels[i]);

                mxml_displayBox.addChild(canvas[i]);
            }
            
            debug("adding video panel " + videoPanels[0]);

            ExternalInterface.addCallback('ping', ping);
            ExternalInterface.addCallback('id_set', idSet);

            ExternalInterface.addCallback("connect", connect);
            ExternalInterface.addCallback('send_me', publishMe);
            ExternalInterface.addCallback('get_them', subscribeThem);

            ExternalInterface.addCallback('camera_select', cameraSelect);
            ExternalInterface.addCallback('camera', cameraGet);
            ExternalInterface.addCallback('cameras', cameraListGet);
            ExternalInterface.addCallback('camera_stop', cameraStop);

            // ExternalInterface.addCallback('set_camera', cameraStop);

            ExternalInterface.addCallback('get_microphone', micGet);
            ExternalInterface.addCallback('get_microphones', micListGet);
            ExternalInterface.addCallback('set_microphone', micSelect);
            ExternalInterface.addCallback('start_mic', micStart);
            ExternalInterface.addCallback('stop_mic', micStop);
            ExternalInterface.addCallback('stop_audio', audioStop);
            ExternalInterface.addCallback('start_audio', audioStart);
            ExternalInterface.addCallback('mic_activity', micActivity);
            ExternalInterface.addCallback('mic_gain', micGain);
            ExternalInterface.addCallback('mic_rate', micRate);
            ExternalInterface.addCallback('set_mic_rate', micRateSet);
            
            // Add the requested JS interface
            javascript_method_flash_initialized = flashVars["flash_inited"];
            javascript_method_flash_connected = flashVars["flash_connected"];
            javascript_method_flash_novidstream = flashVars["flash_novidstream"];;
            javascript_method_flash_failed = flashVars["flash_failed"];
            javascript_method_flash_disconnected = flashVars["flash_disconnected"];
            javascript_method_flash_vidstream = flashVars["flash_vidstream"];
            javascript_method_flash_notify = flashVars["flash_notify"];
            javascript_method_flash_published = flashVars["flash_published"];
            javascript_method_flash_subscribed = flashVars["flash_subscribed"];
            javascript_method_flash_cameraon = flashVars["flash_cameraon"];
            javascript_method_set_hash_id = flashVars["set_hash_id"];

            debug("vid client initialized");
            if (javascript_method_flash_initialized != null) {
                ExternalInterface.call(javascript_method_flash_initialized);
            }
        }

        /** Set the user's id.
         */
        public function idSet(id:String):void {
            myId = id;
        }

        /** Connect the local video to the backend RTMP server and url.
         */
        public function connect(server:String, path:String):void {
            debug("connect called");
            // Make the static netconnection
            nc = new NetConnection();

            // Setup an event listener
            var statusEvent:Object = new Object();
            statusEvent.conStatus = function (infoObject:NetStatusEvent):void {
                var _debug:Function = function (extra:String):void {
                    debug("connect.conStatusHandler: " + infoObject.info.code + " " + extra);
                }
                
                if (infoObject.info.code == "NetConnection.Connect.Failed") {
                    _debug("failed to rtmp://" + server + path);
                }
                else if (infoObject.info.code == "NetConnection.Connect.Rejected") {
                    _debug(infoObject.info.description);
                }
                else {
                    _debug("connected!");
                    ExternalInterface.call(javascript_method_flash_connected);
                }
            }

            // Add the event handler ...
            nc.addEventListener(NetStatusEvent.NET_STATUS, statusEvent.conStatus);
            // ... and connect - this causes the event handler to be called
            nc.connect("rtmp://" + server + path);
        }

        public function myStreamStop():void {
            debug("myStreamStop");
            if (myStream != null) {
                // here we are shutting down the connection to the server
                myStream.attachCamera(null);
                myStream.attachAudio(null);
                myStream.publish(null);
                myStream.close();
                myStream = null;
            }
        }

        /** Stop the selected camera from sending.
         */
        private function cameraStop():void
        {
            if (myStream != null) {
                myStream.attachCamera(null);
            }
            video.attachCamera(null);
            video.clear();
            debug("cameraStop!");
        }
        
        /** Selects the internal camera from a user suggestion.
         *
         * Or just the default camera.
         */
        public function cameraSelect(suggestedCamera:String):void {
            try {
                var cameraIdx:int = Camera.names.indexOf("USB Video Class Video");
                var suggestedCameraIdx:int = Camera.names.indexOf(suggestedCamera);
                cameraIdx = (suggestedCameraIdx > -1) ? suggestedCameraIdx:cameraIdx;
                camera = Camera.getCamera(Camera.names[cameraIdx]);
                if (camera == null) {
                    camera = Camera.getCamera();
                }
            }
            catch (e:Error) {
                debug("cameraSelect:: whoops! camera set failure " + e.message);
            }
            finally {
                debug("cameraSelect:: camera selected == " + camera);
                // Capture the streaming state and then stop.
                var cameraSending:Boolean = (myStream != null);
                //  cameraStop();

                var camStarted:Function = function ():void {
                    debug("cameraSelect.camStarted!");
                };
                
                camera.addEventListener(ActivityEvent.ACTIVITY, camStarted);

                // Now make the container...
                var cameraContainer:VideoDisplay = videoPanels[0] as VideoDisplay;
                var w:int = cameraContainer.width;
                var h:int = cameraContainer.height;
                debug("cameraSelect:: width " + w + " height " + h);
                video = new Video(w, h);
                cameraContainer.addChild(video);

                // Setup the sending stuff
                var aspectRatio:Number = 3/5;
                var framesPS:int = 15;
                camera.setMode(int(w / aspectRatio),
                               int(h / aspectRatio),
                               framesPS, 
                               true);
                camera.setMotionLevel(0);
                camera.setQuality(0, 40);
                camera.setKeyFrameInterval(10);

                debug("cameraSelect:: camera initialized");

                if (cameraSending) {
                }
                else {
                    debug("cameraSelect:: adding to video");
                    video.attachCamera(camera);
                }
            }
        }

        public function cameraGet():String {
            if (camera) {
                return camera.name;
            }
            else {
                return "";
            }
        }

        public function cameraListGet():String {
            return Camera.names.join(",");
        }

        /** Sets up the stream sending video data to the Internet.
         */ 
        private function publishMe(streamId:String):void
        {
            debug("publishMe " + streamId);
            try {
                // Sending the local camera and mic
                var pubState:Object = new Object();
                pubState.onStatus = function (infoObject:NetStatusEvent):void {
                    var _debug:Function = function (extra:String):void {
                        debug("publishMe[" + streamId + "].onStatus: " + infoObject.info.code + " " + extra);
                    }

                    if (infoObject.info.code == "NetStream.Play.StreamNotFound" 
                        || infoObject.info.code == "NetStream.Play.Failed") {
                        _debug(infoObject.info.description);
                    }
                    else if (infoObject.info.code == "NetStream.Publish.Start") {
                        _debug("start!");
                        ExternalInterface.call(javascript_method_flash_published);
                    }
                };
                
                if (myStream == null) {
                    myStream = new NetStream(nc);
                    myStream.addEventListener(NetStatusEvent.NET_STATUS, pubState.onStatus);
                    var publishId:String = "vidclient::" + streamId;
                    debug("publishMe as " + publishId);
                    myStream.publish(publishId);
                    myStream.attachCamera(camera);
                    myStream.attachAudio(microphone);
                }
            }
            catch (e:Error) {
                debug("publishMe error: " + e.toString());
            }
        }

        public function subscribeThem(streamId:String):void {
            // Add a slot from a user
            debug("subscribeThem");

            var videoContainer:VideoDisplay = videoPanels[1] as VideoDisplay;
            var v:Video;
            var canvasContainer:Canvas;
            var subStream:NetStream;

            // Remove any existing
            if (videoContainer != null) {
                debug("subscribeThem found existing container");
                v = videos[1] as Video;
                if (v) {
                    debug("subsribeThem found existing video: " + v.name);

                    canvasContainer = canvas[1] as Canvas;
                    canvasContainer.removeChildAt(1);
                    subStream = netStreams[1] as NetStream;
                    if (subStream != null) {
                        debug("subscribeThem found existing netstream " + subStream);
                        subStream.play(null);
                        subStream.close();
                        subStream = null;
                    }

                    // Close this stuff off whatever
                    v.attachNetStream(null);
                    v.clear();
                }
            }

            // Setup new one.
            videoContainer = videoPanels[1] as VideoDisplay;
            debug("subscribeThem vid container = " + videoContainer);
            v = new Video(videoContainer.width, videoContainer.height);
            v.name = "videoSlot";

            try {
                debug("subscribeThem about to connect the NetStream " + nc);
                // Receiving the remote stream
                subStream = new NetStream(nc);
                var playStatus:Object = new Object();
                playStatus.onStatus = function(infoObject:NetStatusEvent):void {
                    var _debug:Function = function (extra:String):void {
                        debug("subscribeThem[" + streamId + "].onStatus: " + infoObject.info.code + " " + extra);
                    }

                    if (infoObject.info.code == "NetStream.Play.StreamNotFound") {
                        _debug("not found");
                    }
                    else if (infoObject.info.code == "NetStream.Play.Failed") {
                        _debug("failed");
                    }
                    else if (infoObject.info.code == "NetStream.Play.UnpublishNotify") {
                        _debug("nsPlayOnStatus netstream unpublished");
                    }
                    else if (infoObject.info.code == "NetStream.Play.Start") {
                        _debug("started");
                    }
                    else if (infoObject.info.code == "NetStream.Play.PublishNotify") {
                        debug("published");
                        ExternalInterface.call(javascript_method_flash_subscribed);
                    }
                };

                subStream.addEventListener(NetStatusEvent.NET_STATUS, playStatus.onStatus);
                // subStream.bufferTime = 0;
                // subStream.soundTransform.volume = volume.value / 100;
                var connectStr:String = "vidclient::" + streamId;
                debug("subscribeThem playing " + connectStr);
                subStream.play(connectStr);
                v.attachNetStream(subStream);
                videoContainer.addChild(v);
                canvasContainer = canvas[1] as Canvas;
                netStreams[1] = subStream;
                videos[1] = v;
            }
            catch (e:Error) {
                debug("subscribeThem caught error: " + e.toString());
            }
        }

        public function ping ():void {
            debug("ping:: video panel " + videoPanels[0]);
            debug("pong");
        }

        public function micGet():String {
            if (microphone) {
                return microphone.name;
            }
            else {
                return "";
            }
        }

        public function micListGet():String {
            return Microphone.names.join(",");
        }

        public function micSelect(name:String):void {
            debug("micSelect called with " + name);
            // First capture state
            var muteState:Boolean = muted;
            // Then mute and stop and switch...
            micStop();
            var index:int = Microphone.names.indexOf(name);
            microphone = Microphone.getMicrophone(index);
            // Now start if we weren't muted
            if (!muted) {
                micStart();
            }
        }

        public function micActivity():Number {
            if (microphone) {
                return microphone.activityLevel;
            }
            else {
                return 0;
            }
        }

        public function micGain():Number {
            if (microphone) {
                return microphone.gain;
            }
            else {
                return 0;
            }
        }

        public function micRate():Number {
            if (microphone) {
                return microphone.rate;
            }
            else {
                return 0;
            }
        }

        public function micRateSet(rate:int):void {
            if (microphone) {
                microphone.rate = rate;
            }
        }

        public function micStop():void {
            debug("micStop");
            if (myStream != null) {
                muted = true;
                if (microphone) {
                    microphone.gain = 0;
                }
                // myStream.attachAudio(null);
            }
        }

        public function micStart():void {
            debug("startMic");
            if (microphone == null) {
                debug("micStart failed; call micSelect first");
            }
            else {
                microphone.setSilenceLevel(0);
                microphone.setUseEchoSuppression(true);
                microphone.rate = 11;
                microphone.gain = 30;
                muted = false;
                if (myStream != null) {
                    myStream.attachAudio(microphone);
                }
            }
        }

        public function audioStop():void
        {
            debug("audioStop: setting volume to 0");
            SoundMixer.soundTransform = new SoundTransform(0);
        }

        public function audioStart():void
        {
            debug("audioStart");
            SoundMixer.soundTransform = new SoundTransform(1);
        }


        private function debug(msg:String):void 
        {
            ExternalInterface.call(javascript_method_log, "DEBUG: " + msg);
        }
    }    
}
