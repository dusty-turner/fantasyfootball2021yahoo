library(httr)
library(XML)
library(httpuv)
options("httr_oob_default" = T)

# Setup the app


b_url <- "https://fantasysports.yahooapis.com" #base url

#Create Endpoint
yahoo <- httr::oauth_endpoint(authorize = "https://api.login.yahoo.com/oauth2/request_auth"
                              , access = "https://api.login.yahoo.com/oauth2/get_token"
                              , base_url = b_url)
#Create App
myapp <- httr::oauth_app("yahoo", key=Sys.getenv("cKey"), secret = Sys.getenv("cSecret"),redirect_uri = "oob")

#Open Browser to Authorization Code
httr::BROWSE(httr::oauth2.0_authorize_url(yahoo, myapp, scope="fspt-r"
                                          , redirect_uri = myapp$redirect_uri))

#Create Token
# yahoo_token <- httr::oauth2.0_access_token(yahoo,myapp,code="uhfc6uz")
# save(yahoo_token,file="yahoo_token.Rdata")
