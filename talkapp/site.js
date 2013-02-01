// Main js
var talkapp =
    (function () {
         var debug = document.location.search.indexOf("debug") >= 0;
         var video_server = $("#videoserver").text();
         var me = $("#myemail").text();
         var my_nick = $("#mynick").text();
         var blurred = false;
         var audio_cookie = "teamchat-audio";
         var chat_poll_time = 350 * 1000;
         var chat_poll_timer = null;
         var messages_callback = null;
         if (debug) { console.log("video-server: " + video_server); }

         var toggle_debug = function (state) {
             if (state === undefined) {
                 // Just toggle
                 debug = ! debug;
             }
             else {
                 debug = state;
             }
         };

         // Init the carousel
         var carousel_boot = function () {
             $("#carousel").carouFredSel(
                 {
                     items: {
                         visible: 2,
                     },
                     cookie: true,
                     align: "right",
                     height: "auto",
                     auto: {
                         play: false,
                     },
                     prev: {
                         // key: "left",
                         items: 1,
                     },
                     next: {
                         // key: "right",
                         items: 1,
                     }
                 }
             );
             $(".carousel-control").filter(".left").on(
                 "click", 
                 function (evt) {
                     $("#carousel").trigger("prev");
                     return false;
                 }
             );
             $(".carousel-control").filter(".right").on(
                 "click", 
                 function (evt) {
                     $("#carousel").trigger("next");
                     return false;
                 }
             );
         };

         // Setup some functions
         var service = function () {
             // FIXME needs to share code with the init call to /session/
             $.ajax({ url: "/user/session/?start=1",
                      dataType: "json",
                      success: function (data, status) {
                          if (debug) { console.log("we started the service with: " + data); }
                          if (data["session"] == true
                              || data["error"] == "already started") {
                              $("#status-disconnected").addClass("hidden");
                              $("#status-connected").removeClass("hidden");
                              $("#connect-to-chat").removeClass("hidden");
                          }
                          else {
                              $("#status-connected").addClass("hidden");
                              $("#status-disconnected").removeClass("hidden");
                              $("#connect-to-chat").addClass("hidden");
                          }
                      }
                    });
         };

         // Convert text urls into A link HTML
         var url_it = function (text) {
             var exp=/(http[s]*:\/\/[^ <>]+)/ig;
             return text.replace(
                 exp,
                 function (match) {
                     if (debug) { console.log("link text = " + match + " (" + match.length + ")"); }
                     // if the url is too long then truncate it as content
                     var length = 30;
                     var content
                         = (match.length > length)
                         ? match.substring(0, length) + "..."
                         : match;
                     return "<a target='_blank' href='" + match + "'>"
                         + content
                         + "</a>";
                 }
             );
         };

         // Convert urls into A links
         var urlize = function (selector) {
             $(selector).html(
                 url_it($(selector).html())
             );
         };

         // Convert an email to an id
         var email2id = function (email, prefix) {
             var str_pre = (prefix != null) ? prefix : "";
             var key_id = str_pre
                 + email.replace("@", "at").replace(/\./g, "-", "g");
             return key_id;
         };

         // Attach the event to the call button
         var on_call = function (to_email, a_id) {
             $("#" + a_id).on(
                 "click",
                 function (evt) {
                     var time = Date.now();
                     $.ajax(
                         { url: "/user/vidcall/",
                           data: { to: to_email, time: time },
                           dataType: "text",
                           success: function (data, status) {
                               if (debug) { console.log("video-call got " + data); }
                           }
                         }
                     );
                     video.display(
                         video_server,
                         me + time,
                         to_email + time,
                         function () { // unpublished handler
                             $("#videocall").addClass("hidden");
                         }
                     );
                     if (debug) { console.log("video-call " + me + " " + to_email); }
                 }
             );
         };

         var do_cough = function () {
             var audio = $.cookie(audio_cookie);
             if (debug) { console.log("do_cough audio = " + audio); }
             if (audio == "true") {
                 var audioElement = document.createElement('audio');
                 audioElement.setAttribute('src', '/-/2012-11-05-220508.ogg');
                 audioElement.play();
             }
         };

         var do_incomming = function () {
             var audioElement = document.createElement('audio');
             audioElement.setAttribute('src', '/-/video-incomming-sound.ogg');
             audioElement.play();
         };


         // The list of emails in the page - email: md5(lower(email))
         var emails = {};

         var gravatarize = function () {
             $.each(
                 emails,
                 function (key, arr) {
                     if (debug) { console.log("gravatarize email = " + key); }
                     var grav_url
                         = "http://www.gravatar.com/avatar/" + arr;
                     var abbr = $("#emails abbr[title='" + key + "']");
                     if (abbr.length < 1) {
                         $("<abbr title='" + key + "'/>").appendTo(
                             $("#emails")
                         );
                         abbr = $("#emails abbr[title='" + key + "']");
                     }
                     var email_id = email2id(key);
                     var a_id = "call-" + email_id;
                     abbr.html(
                         "<img src='" + grav_url + "'/>"
                             + ((key != me) ?
                                ("<a id='" + a_id + "'"
                                 + " href='javascript:;' "
                                 + " class='btn btn-small btn-primary'>"
                                 + "call</a>"
                                 + "<input class='hidden'"
                                 + " style='display: none'"
                                 + " type='checkbox' "
                                 + " name='" + email_id + "' "
                                 + " value='" + key + "'/>") : "")
                     );
                     if (key != me) {
                         on_call(key, a_id);
                         var abbr_img = $("img", abbr);
                         var saved_placeholder;
                         $(abbr_img).on(
                             {
                                 mouseenter:
                                 function (evt) {
                                     abbr.addClass("nick-select-hover");
                                     saved_placeholder = 
                                         $("#channels input").attr("placeholder");
                                     $("#channels input").attr(
                                         "placeholder", "create a private chatroom"
                                     );
                                 },
                                 mouseout: 
                                 function (evt) {
                                     abbr.removeClass("nick-select-hover");
                                     if (!abbr.hasClass("nick-selected")) {
                                         $("#channels input").attr(
                                             "placeholder", saved_placeholder
                                         );
                                     }
                                 },
                                 click:
                                 function (evt) {
                                     abbr.toggleClass("nick-selected");
                                     // This doesn't toggle which it should.
                                     $("#channels input").attr(
                                         "placeholder", "create a private chatroom"
                                     );
                                     // Toggle the state of the associated radio
                                     var state = $("input[name=" + email_id + "]").prop("checked");
                                     $("input[name=" + email_id + "]").prop("checked", !state);
                                 }
                             }
                         );
                     }
                 }
             );
             if ($(emails).length > 0) {
                 $("#gravatars").removeClass("hidden");
             }
         };

         // Initialize the email list
         $.each(
             $("#emails abbr"),
             function (key, arr) {
                 emails[arr.title] = hex_md5(arr.title.toLowerCase());
             }
         );
         gravatarize();


         // Process a video call from someone else, received via comet
         var do_videocall = function (data) {
             if (debug) { console.log("videocall data = " + data); }
             var from = data[0];
             var to = data[1];
             var time = data[2];

             $("#videoaccept button").on(
                 "click", 
                 function (evt) {
                     // Hide the accept panel
                     $("#videoaccept").addClass("hidden");
                     // Unique the from and to with the time
                     video.display(video_server, from + time, to + time);
                 }
             );
             do_incomming();
             $("#videoaccept").removeClass("hidden");
         };

         var do_users = function (data) {
             $.each(
                 data,
                 function (key,arr) {
                     if (debug) { console.log("do_users key " + key + " arr " + arr); }
                     if (arr == "offline") {
                         delete emails[key];
                         $("#emails abbr[title='" + key + "']").remove();
                     }
                     else if (arr == "online") {
                         emails[key] = hex_md5(key.toLowerCase());
                     }
                 }
             );
             gravatarize();
         };

         var msg_template = function (date, username, message) {
             return $("<tr id='" + date + "'>"
                      + "<td class='username " + username + "'>"
                      + username + "</td>"
                      + "<td class='message'>" + url_it(message) + "</td>"
                      + "</tr>");
         };

         var do_messages = function (data, channel) {
             if (debug) { console.log("messages " + data + " channel " + channel); }
             if (!channel) {
                 channel = "";
             }
             else {
                 channel = channel + " ";
             }

             // and display each one
             if (data != null && $(data).length > 0) {
                 $.each(data,
                        function (key, arr) {
                            var looked_up = $("#" + key);
                            if (looked_up.length < 1) {
                                var username = arr[0];
                                var message = arr[1];
                                if (debug) { console.log("messages " + username + " " + message); }
                                msg_template(
                                    key, username, message
                                ).insertBefore(channel + "table tr:first-child");
                                if (debug) { console.log("messages call cough? " + blurred); }
                                if (blurred && username != my_nick) {
                                    do_cough();
                                }
                            }
                            else {
                                if (debug) { console.log("already got that one"); }
                            }
                        });
             }
             if ($.isFunction(messages_callback)) {
                 messages_callback();
                 messages_callback = null;
             }
         };

         // Allow the time to be updated
         var chat_poll_time_set = function (time) {
             chat_poll_time = time;  
             if (chat_poll_timer != null) {
                 clearTimeout(chat_poll_timer);
             }
         };

         var time_str = function (date) {
             var hours = "" + date.getHours();
             var mins = "" + date.getMinutes();
             if (hours.length < 2) {
                 hours = "0" + hours;
             }
             if (mins.length < 2) {
                 mins = "0" + mins;
             }
             var time_string = hours + ":" + mins;
             return time_string;
         };

         var chat_time = function () {
             if ($("table tr:first-child").attr("class") != "chat_time") {
                 var date = new Date();
                 var last_time 
                     = parseInt($(".chat_time").first().attr("data-time"));
                 if (debug) { console.log("chat_time " + last_time); };
                 if (isNaN(last_time) 
                     || date.getTime() > (last_time/1000) + (15*60)) {
                     $("<tr class='chat_time' "
                       + "data-time='" +  date.getTime() + "'>"
                       + "<td colspan='2'>" 
                       + time_str(date)
                       + "</td></tr>"
                      ).insertBefore("table tr:first-child");
                 }
             }
             else {
                 if (debug) { console.log("chat_time - skipping"); };
             }
         };

         var chat_poll = function () {
             $.ajax(
                 { url: '/user/poll/',
                   dataType: "jsonp",
                   timeout: chat_poll_time,
                   success: function (data, status) {
                       // data key can be "message" or "user" or
                       // something else like "video"
                       $.each(data,
                              function (key, arr) {
                                  if (debug) { console.log("chat_poll cb key=" + key + " arr=" + arr); }
                                  if (key == "message") {
                                      do_messages(arr);
                                  }
                                  else if (key == "user") {
                                      do_users(arr);
                                  }
                                  else if (key == "video") {
                                      do_videocall(arr);
                                  }
                                  else {
                                      // Should be debug really
                                      console.log("unknown message: "
                                                  + "key=" + key
                                                  + " arr=" + arr);
                                  }
                              }
                             );
                   },
                   error: function (jqXHR, status) {
                       if (debug) { console.log("chat_poll returned status " + status); }
                       chat_time();
                   },
                   complete: function(jqXHR, status) {
                       // restart even if we failed
                       if (status == "error") {
                           // How do we find out if we're disconnected?
                       }
                       chat_poll_timer = setTimeout(chat_poll, 100);
                   }
                 }
             );
         };

         // For pulling back a lot of messages
         var channel_messages = function (channel) {
             var url = "/user/channeldata/";
             if (typeof channel != "undefined") {
                 url = url + "/" + channel + "/";
             }
             if (debug) { console.log("channel_messages for: " + url); }
             $.ajax(
                 { url: url,
                   // We could use this for date?
                   // data: { "channel-name": channel },
                   dataType: "jsonp",
                   success: function (data, status) {
                       if (debug) { console.log("channel_messages back for: " + url); }
                       do_messages(data, channel);
                   },
                   complete: function (jqXHR, status) {
                       console.log("ajax compeltion status = " + status);
                   }
                 }
             );
         };

         // Form handling
         var form_queue = new Array();

         var form_queue_list = function () { return form_queue; };

         var form_queue_process = function (form_queue) {
             var head = form_queue.shift();
             while (!(head === undefined)) {
                 var action = head[0];
                 var data = head[1];
                 var cb = head[2];
                 $.post(
                     action, data,
                     function () { 
                         try { if ($.isFunction(cb)) { cb(); } } 
                         catch (x) {
                             if (debug) { 
                                 console.log(
                                     "form_queue_process success cb failed ", e
                                 );
                             }
                         }
                     }
                 );
                 head = form_queue.shift();
             }
         };

         var form_attach = function (selector, callback) {
             if (debug) { console.log("form_attach connecting forms"); }
             var action = $(selector).attr("action");
             $(selector).submit(
                 function (evt) {
                     if (debug) { console.log("form_attach target: ", evt.target); }
                     var data = $(evt.target).serialize();
                     $(evt.target)[0].reset();
                     if (debug) { console.log("form_attach data: ", data); }
                     form_queue.push([action, data, callback]);
                     form_queue_process(form_queue);
                     return false;
                 }
             );
         };

         var channel_open = function (channel) {
             console.log("channel open " + channel);
             $("#carousel").trigger(
                 "insertItem",
                 "<div id='"
                     + channel 
                     + "' class='span8 channel'>"
                     + "<h4>" + channel + "</h4>"
                     + "<table class='chat'><tr/></table>"
                     + "</div>"
             );
             $("#chat-panel form").clone().insertAfter(
                 "#" + channel + " h4"
             );
             $("#" + channel + " h4 + form input[name=channel-name]").attr("value",channel);
             $("#carousel").trigger("next");
             form_attach(".channel-send");
             channel_messages(channel);
         };

         var channels = function () {
             $.ajax(
                 { url: '/user/channel/',
                   dataType: "jsonp",
                   success: function (data, status) {
                       $.each(
                           data,
                           function (key, arr) {
                               $("<li>" 
                                 + "<a class='channel-link' "
                                 + " href='javascript:;'>"
                                 + arr 
                                 + "</a></li>"
                                ).appendTo("#channel-list");
                           }
                       );
                       $(".channel-link").on(
                           "click",
                           function (evt) {
                               channel_open(evt.target.text.substring(1));
                           }
                       );
                   }
                 }
             );
         };

         // If we have an end-call button bind it
         var end_call = $("#end-call");
         if (end_call.length > 0) {
             end_call.on(
                 "click",
                 function (evt) {
                     $("#videocall").addClass("hidden");
                 }
             );
         }

         // Bind the send form stuff, if it's on the page
         var send_form_target = $("[name=_sendtarget]");
         if (send_form_target.length > 0) {
             send_form_target.load(
                 function (evt) {
                     $("[name=msg]")[0].value = "";
                     $("[name=msg]").focus();
                 });
         }

         // If we should start the chat poller
         if (typeof talkapp_do_chat != "undefined") {
             $("#ctrl-bar input[name=cough]").prop(
                 "checked",
                 $.cookie(audio_cookie)
             );
             $("#ctrl-bar input[name=cough]").on(
                 "change",
                 function (evt) {
                     var is_on = $(evt.target).prop("checked");
                     $.cookie(audio_cookie, is_on, { expires: 365, path: '/user/chat/' });
                 }
             );

             // Set the messages callback to something that will update
             messages_callback = function (){
                 var first_chat_row = $("table tr").last().prev();
                 if (first_chat_row.length > 0) {
                     var d = (/([0-9:-]+):[0-9]+/.exec(first_chat_row.attr("id")));
                     if (d != null) {
                         $(".chat_time td").first().text(d[1]);
                     }
                 }
                 else {
                     $(".chat_time td").first().text(time_str(new Date()));
                 }
             };
             channel_messages();

             channels();

             // Make the chat poller run
             chat_poll_timer = setTimeout(chat_poll, 1000);
             // also urlize the chat panel everything
             urlize("#chat-panel");
             // also collect gravatars
             gravatarize();
             // also turn on blur notifications
             $(window).on(
                 "blur", 
                 function (evt) { 
                     if (debug) { console.log("blur - on"); }
                     blurred = true;
                 }
             );
             $(window).on(
                 "focus", 
                 function (evt) { 
                     if (debug) { console.log("blur - off"); }
                     blurred = false; 
                 }
             );
             // also turn on the carousel
             if (true) {
                 $(document).ready(carousel_boot);
                 $(document).ready(
                     function (evt) {
                         form_attach(".channel-send");
                     }
                 );
             }
         }



         // /user/ page stuff

         // initialize the value of the status
         var status_connected = $("#status-connected");
         var status_disconnected = $("#status-disconnected");
         if (status_connected.length > 0 || status_disconnected.length > 0) {
             console.log("found status dom");
             $.ajax({ url: "/user/session/",
                      dataType: "json",
                      success: function (data, status) {
                          if (data["session"] == true) {
                              $("#status-disconnected").addClass("hidden");
                              $("#status-connected").removeClass("hidden");
                              $("#connect-to-chat").removeClass("hidden");
                          }
                          else {
                              $("#status-connected").addClass("hidden");
                              $("#status-disconnected").removeClass("hidden");
                              $("#connect-to-chat").addClass("hidden");
                          }
                      }
                    });
         }

         // If we have a connect button add the link
         var connect_button = $("#connect");
         if (connect_button.length > 0) {
             connect_button.on(
                 "click", function (evt) {
                     service();
                 });
         }

         // Display the SSH keys
         var do_keys_show = function () {
             $("#keys .panel table").empty();
             var keyrow = function (name, keytext) {
                 keytext = keytext.substring(0,35) + "...";
                 $("<tr>"
                   + "<td>" + name + "</td>"
                   + "<td>" + keytext + "</td></tr>\n"
                  ).appendTo("#keys .panel table");
             };
             console.log("do_keys_show displaying keys");
             $.ajax(
                 { url: "/user/keys/",
                   dataType: "jsonp",
                   success:
                   function (data, status) {
                       if (data) {
                           $.each(data, keyrow);
                       }
                   }
                 }
             );
         };

         // Keys panel
         var keys_show = $("#keys .reveal");
         if (keys_show.length > 0) {
             keys_show.on(
                 "click",
                 function (evt) {
                     $("#keys .reveal").addClass("hidden");
                     do_keys_show();
                     form_attach(".keysform", do_keys_show);
                     $("#keys .panel").removeClass("hidden");
                 }
             );
         }

         // Return public API in an object
         return {
             toggle_debug: toggle_debug,
             chat_poll_time_set: chat_poll_time_set,
             channels: channels
         };
     })();


// End
