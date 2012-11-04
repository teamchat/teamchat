// Main js
var talkapp =
    (function () {
         var debug = document.location.search.indexOf("debug") >= 0;
         var video_server = $("#videoserver").text();
         var me = $("#myemail").text();
         if (debug) { console.log("video-server: " + video_server); }

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
                         key: "left",
                         items: 1,
                     },
                     next: {
                         key: "right",
                         items: 1,
                     }
                 }
             );
         };

         // Setup some functions
         var service = function () {
             // FIXME needs to share code with the init call to /session/
             $.ajax({ url: "/session/?start=1",
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

         var config = function () {
             $.ajax({ url: "/config/",
                      dataType: "text",
                      success: function (data, status) {
                          if (debug) { console.log("we configured with: " + data); }
                          service();
                      }
                    });
         };

         // Convert text urls into A link HTML
         var url_it = function (text) {
             var exp=/(http:\/\/[^ <>]+)/ig;
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
                         { url: "/vidcall/",
                           data: { to: to_email, time: time },
                           dataType: "text",
                           success: function (data, status) {
                               if (debug) { console.log("video-call got " + data); }
                           }
                         }
                     );
                     video.display(video_server, me + time, to_email + time);
                     if (debug) { console.log("video-call " + me + " " + to_email); }
                 }
             );
         };

         // The list of emails in the page - email: md5(lower(email))
         var emails = {};

         var gravatarize = function () {
             $.each(emails,
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
                        var a_id = email2id(key, "call-");
                        abbr.html(
                            "<img src='" + grav_url + "'/>"
                                + "<a id='" + a_id + "'"
                                + " href='javascript:;' "
                                + " class='btn btn-small btn-primary'>call</a>"
                        );
                        on_call(key, a_id);
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
             // Unique the from and to with the time
             video.display(video_server, from + time, to + time);
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

         var do_messages = function (data) {
             $.each(data,
                    function (key, arr) {
                        var looked_up = $("#" + key);
                        if (looked_up.length < 1) {
                            var username = arr[0];
                            var message = arr[1];
                            msg_template(
                                key, username, message
                            ).insertBefore("table tr:first-child");
                        }
                        else {
                            if (debug) { console.log("already got that one"); }
                        }
                    });
         };

         var chat_poll = function () {
             $.ajax(
                 { url: '/poll/',
                   dataType: "jsonp",
                   timeout: 350 * 1000,
                   success: function (data, status) {
                       // data key can be "message" or "user" or
                       // something else like "video"
                       console.log(data);
                       $.each(data,
                              function (key, arr) {
                                  if (debug) { console.log("key=" + key + " arr=" + arr); }
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
                       if (debug) { console.log("poll returned status " + status); }
                   },
                   complete: function(jqXHR, status) {
                       // restart even if we failed
                       if (!debug) {
                       }
                       setTimeout(chat_poll, 100);
                   }
                 }
             );
         };

         // initialize the value of the status
         var status_connected = $("#status_connected");
         var status_disconnected = $("#status_disconnected");
         if (status_connected.length > 0 || status_disconnected.length > 0) {
             console.log("found status dom");
             $.ajax({ url: "/session/",
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

         // If we should configure the user then do it
         if (typeof talkapp_do_config != "undefined") {
             config();
         }

         // If we should start the chat poller
         if (typeof talkapp_do_chat != "undefined") {
             setTimeout(chat_poll, 1000);
             // also urlize the chat panel everything
             urlize("#chat-panel");
             // also collect gravatars
             gravatarize();
             // also turn on the carousel
             if (true) {
                 //$(document).ready(carousel_boot);
             }
         }

         // Return public API in an object
         return {
         };
     })();


// End
