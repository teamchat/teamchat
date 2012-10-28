// Main js
var talkapp = 
    (function () {
         // Do some page init

         // Setup some functions
         var service = function () {
             // FIXME needs to share code with the init call to /session/
             $.ajax({ url: "/session/?start=1",
                      dataType: "json",
                      success: function (data, status) {
                          console.log("we started the service with: " + data);
                          if (data["session"] == true 
                              || data["error"] == "already started") {
                              $("#status-disconnected").addClass("hidden");
                              $("#status-connected").removeClass("hidden");
                          }
                          else {
                              $("#status-connected").addClass("hidden");
                              $("#status-disconnected").removeClass("hidden");
                          }
                      }
                    });
         };

         var config = function () {
             $.ajax({ url: "/config/",
                      dataType: "text",
                      success: function (data, status) {
                          console.log("we configured with: " + data);
                          service();
                      }
                    });
         };

         // Convert text urls into A link HTML
         var url_it = function (text) {
             var exp=/(http:\/\/[^ <>]+)/ig;
             return text.replace(exp, "<a target='_blank' href='$1'>$1</a>");
         };

         // Convert urls into A links
         var urlize = function (selector) {
             $(selector).html(
                 url_it($(selector).html())
             );
         };
         
         var chat_poll = function () {
             var template = function (username, message) {
                 return $("<tr>"
                          + "<td class='username " + username + "'>" + username + "</td>"
                          + "<td class='message'>" + url_it(message) + "</td>"
                          + "</tr>");
             };
             $.ajax(
                 { url: '/poll/',
                   dataType: "jsonp",
                   success: function (data, status) {
                       $.each(data,
                              function (key, arr) {
                                  var username = arr[0];
                                  var message = arr[1];
                                  template(username,
                                           message).insertBefore("table tr:first-child");
                              });
                   },
                   error: function (jqXHR, status) {
                       console.log("poll returned status " + status);
                   },
                  complete: function(jqXHR, status) {
                      // restart even if we failed                                                                                                    
                      setTimeout(chat_poll, 100);
                  }
                 }
             );
         };

         // The list of emails in the page - email: md5(lower(email))
         var emails = {};
         var gravatarize = function () {
             $.each($("abbr"), 
                    function (key, arr) {
                        if (!emails[arr.title]) {
                            emails[arr.title] = hex_md5(arr.title.toLowerCase());
                        }
                    }
                   );
             $.each(emails,
                    function (key, arr) {
                        var grav_url = "http://www.gravatar.com/avatar/" + arr;
                        console.log(grav_url);
                        var html = "<abbr title='" + key + "'>"
                            + "<img src='" + grav_url + "'/></abbr>";
                        $("#gravatars").append($(html));
                    }
                   );
             if ($(emails).length > 0) {
                 $("#gravatars").removeClass("hidden");
             }
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
                          }
                          else {
                              $("#status-connected").addClass("hidden");
                              $("#status-disconnected").removeClass("hidden");
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
         }

         // Return public API in an object
         return {
         };
     })();


// End
