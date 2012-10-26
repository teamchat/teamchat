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

         // initialize the value of the status
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

         // If we have a connect button add the link
         var connect_button = $("#connect");
         if (connect_button) {
             connect_button.on(
                 "click", function (evt) {
                     service();
                 });
         }
         
         // If we should configure the user then do it
         if (typeof talkapp_do_config != "undefined") {
             config();
         }
         
         // Return public API in an object
         return {
         };
     })();


// End
