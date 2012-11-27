;; Template variables for talkapp

(defconst talkapp-template/head-css "<link rel=\"stylesheet\"
   href=\"/-/bootstrap.css\" type=\"text/css\"/>
<link rel=\"stylesheet\" href=\"/-/style.css\" type=\"text/css\"/>"
  "Template HEAD stuff.")

(defconst talkapp-template/chat-header "<div id=\"chat-header\">
    <div class=\"container\">
        <div class=\"logo\">
            <h4>teamchat.net</h4>
            <p>all your communication needs</p>
            <a href=\"/user/\">[goto your profile]</a>
        </div>
        <h1>TeamChat</h1>
    </div>
</div>"
  "The header for chat pages.")

(defconst talkapp-template/body-header "<div id='chat-header'>
    <div class='container'>
        <div class='logo'>
            <h4>teamchat.net</h4>
            <p>all your communication needs</p>
        </div>
        <h1>TeamChat</h1>
    </div>
</div>
<div class='container'>")

(defconst talkapp-template/body-header-wiki "<div id='chat-header'>
    <div class='container'>
        <div class='logo'>
            <h4>teamchat.net</h4>
            <p>all your communication needs</p>
        </div>
        <h1>TeamChat</h1>
    </div>
</div>
<div class='container wiki'>")


(defconst talkapp-template/body-footer "</div>
<footer class='footer'>
    <div class='container'>
        <p class='pull-right'><a href='#'>Back to top</a></p>
        <p>Copyright (C) 2012 TeamChat</p>
        <ul class='inline-list'>
            <li><a href='/site/about/'>About</a></li>
            <li><a href='/site/developers/'>Developers</a></li>
            <li><a href='/site/contact/'>Contact</a></li>
        </ul>
        <ul class='inline-list'>
            <li><a href='/site/terms/'>Terms and conditions</a></li>
            <li><a href='/site/FAQ/'>FAQ</a></li>
            <li><a target='_new'
                   href='http://facebook.com/teamchatnet'>Facebook</a></li>
            <li><a target='_new'
                   href='http://twitter.com/teamchatnet'>Twitter</a></li>
            <li><a target='_new'
                   href='http://github.com/teamchat/teamchat'>Github</a></li>
        </ul>
    </div>
</footer>
<script src=\"/-/jquery.js\"    language=\"Javascript\"></script>
<script src=\"/-/cookie.js\"    language=\"Javascript\"></script>
<script src=\"/-/swfobject.js\" language=\"Javascript\"></script>
<script src=\"/-/carousel.js\"  language=\"Javascript\"></script>
<script src=\"/-/bootstrap.js\" language=\"Javascript\"></script>
<script src=\"/-/md5.js\"       language=\"Javascript\"></script>
<script src=\"/-/video.js\"     language=\"Javascript\"></script>
<script src=\"/-/site.js\"      language=\"Javascript\"></script>"
  "The body footer used in pages.")

(provide 'talkapp-templates)
