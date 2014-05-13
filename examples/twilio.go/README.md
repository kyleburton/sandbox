# Golang client API for the Twilio API

# TASKS

* JSON Configuration 
 * $HOME/.twilio-go.json
 * BaseUrl, default: https://api.twilio.com/2010-04-01
 * AccountSid
 * AuthToken
* twilio.Client
 * create struct, accepts AccountSid, AuthToken
 * implement GET    (passes AccountSid:Authstruct as http-basic auth)
 * implement PUT    (passes AccountSid:Authstruct as http-basic auth)
 * implement POST   (passes AccountSid:Authstruct as http-basic auth)
 * implement DELETE (passes AccountSid:Authstruct as http-basic auth)
* APIs
 * Calls
  * https://www.twilio.com/docs/api/rest/making-calls
 * Call Queues
 * Messaging (SMS)
 * Phone Numbers
 * Usage
 * Accounts
 * Applications
 * SIP
 * Miscellaneous

# References 

* https://www.twilio.com/docs/api/rest
