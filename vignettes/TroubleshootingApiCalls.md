<!--
%\VignetteEngine{knitr::knitr}
%\VignetteIndexEntry{Troubleshooting REDCap API Calls}
-->

# Troubleshooting REDCap API Calls
There are many links in the pipeline between your institution's [REDCap](http://www.project-redcap.org/) server and the API user.  When the end result is unsuccesful, this document should help narrow the location of the possible problem.  The first two sections will be relevant to almost any language interacting with the API.  The remaining sections are possibly relevant only to your language (eg, Python, R, SAS, bash), or your software library ([redcap](https://github.com/jeffreyhorner/redcap) and [REDCapR](https://github.com/OuhscBbmc/REDCapR) in R and [PyCap](http://sburns.org/PyCap/) in Python).

## Server Configuration and Authorization
This first group of checks primarily focuses on the server and the logins accounts.  Unlike the other sections, REDCap administrator privileges are necessary for most of these checks.

 1. **Does the user have an account for the *server*?**  
 This can be verified in the `Browse Users` section of the server's `Control Center`.
 
 1. **Does the user have permissions for the specific *project*?**  
 This can be verified in the `User Rights` section within the project.  Notice that it's possible (but ultimately not useful) to have an account here, but not with the server, so don't skip the previous step.
 
 1. **Can the user login in normally through the interface?**  
 However if the username and password aren't working, the API theoretically might still work because it uses their token instead of their password.
 
 1. **Has the user verified their account by responding to the automated email sent to them?**  
 This can be verified in the `Browse Users` section of your server's `Control Center`.  For each email address they've entered, there should be a green 'Verified' label and icon.
 
 1. **Has the user been granted the desired import and/or export permissions?**  
 This can be verified in the `User Rights` section or in the `API` section (and it's `Manage All Project Tokens` tab) within the project.  Alternatively, it can be verified in the `API Tokens` section of the server's `Control Center`.
 
 1. **Are they using the correct token?**  
 This can be verified in the `API` section (and it's `Manage All Project Tokens` tab) within the project.  Click the magnifying glass icon in the user's row.  Alternatively, it can be verified in the `API Tokens` section of the server's `Control Center`.

## Language Agnostic API
This section group examines potential problems that occur after it leaves a working server, but before it is handled by their programming language (eg, Python and R).  [Postman](http://www.getpostman.com/) is a Chrome plugin recommended by [several](https://groups.google.com/forum/#!searchin/project-redcap/postman/project-redcap/eYK_SLzW2k4/0hmf8vWLpdAJ) [people](https://groups.google.com/forum/#!searchin/project-redcap/postman/project-redcap/RdLeRFGqzbg/WgdjTBLph1kJ) that makes trouble shooting much more efficient.

 <img src="./images/PostmanScreenshot.png" alt="PostmanScreenshot" style="width: 800px;"/>

 1. **Is Postman installed and operating correctly?**  
 If it helps to start with a different REDCap server, you can use this dummy project containing fake data hosted by the [OUHSC BBMC](http://ouhsc.edu/bbmc/).  The url can be found in the screenshot above.  There are three key-value pairs: (1) the 'token' is `9A81268476645C4E5F03428B8AC3AA7B`, (2) the 'content' is `record`, and (3) the 'format' should be `CSV`.  When checking your own server, the token value should change, but the content and format should not.  It should return five records in a CSV format.  The 'status' should be `200 OK`.  The result should look roughly like:
 
 ```
 record_id,first_name,last_name,address,telephone,email,dob,age,ethnicity,race,sex,height,weight,bmi,comments,demographics_complete
  "1","Nutmeg","Nutmouse","14 Rose Cottage St.
  Kenning UK, 323232","(432) 456-4848","nutty@mouse.com","2003-08-30",10,1,2,0,5,1,400,"Character in a book, with some guessing",2
  "2","Tumtum","Nutmouse","14 Rose Cottage Blvd.
  Kenning UK 34243","(234) 234-2343","tummy@mouse.comm","2003-03-10",10,1,6,1,6,1,277.8,"A mouse character from a good book",2
  "3","Marcus","Wood","243 Hill St.
  Guthrie OK 73402","(433) 435-9865","mw@mwood.net","1934-04-09",79,0,4,1,180,80,24.7,"completely made up",2
  "4","Trudy","DAG","342 Elm
  Duncanville TX, 75116","(987) 654-3210","peroxide@blonde.com","1952-11-02",61,1,4,0,165,54,19.8,"This record doesn't have a DAG assigned
  
  So call up Trudy on the telephone
  Send her a letter in the mail",2
  "5","John Lee","Walker","Hotel Suite
  New Orleans LA, 70115","(333) 333-4444","left@hippocket.com","1955-04-15",58,1,4,1,193.04,104,27.9,"Had a hand for trouble and a eye for cash
  
  He had a gold watch chain and a black mustache",2
 ```
 
 1. **Can an administrator query the API successfully with Postman with the admin token?**  
 As an administrator, create an account for yourself, and verify that your token works on your server and project.
  
 1. **Can an administrator query the API successfully with Postman with the user's token?**  
 Use Postman as before, but replace your token with the user's token.  Once the whole problem is solved, consider reissuing a new API token.
 
 1. **Can an user query the API successfully with Postman with the their own token?**  
 The values they enter should be exactly the same as those entered in the previous step.  A failure here (assuming the previous step was successful) suggests a network or firewall issue.  If the server is behind your instituion's firewall, verify the user is connecting successfully through the VPN.
 
## Calling API from R
There are several ways to call REDCap's API from [R](http://cran.r-project.org/).  The packages [redcap](https://github.com/jeffreyhorner/redcap) and [REDCapR](https://github.com/OuhscBbmc/REDCapR) both rely on the [RCurl](http://cran.r-project.org/web/packages/RCurl) package.
 
 1. **Is RCurl installed on the user's local machine?**  
 If so, running `require(RCurl)` should produce the following output if you're starting with a fresh session of R:
 
 ``` r
  > require(RCurl)
  Loading required package: RCurl
  Loading required package: bitops
 ``` 
 
 1. **Does the user have the most recent version of RCurl?**   
 There are several ways to do this, but the easiest is probably to run `update.packages(ask=FALSE)`.  That optional argument prevents the user from needing to respond 'Y' to updating each outdated package.
 
 1. **Can the user query a *subset* of the REDCap project using RCurl?**   
 Both the [redcap](https://github.com/jeffreyhorner/redcap) and [REDCapR](https://github.com/OuhscBbmc/REDCapR) employ something similar to the following function in [RCurl](http://cran.r-project.org/web/packages/RCurl).  If you're curious, here is the relevant source code for [https://github.com/jeffreyhorner/redcap/blob/master/R/exportRecords.R](redcap) and [REDCapR](https://github.com/OuhscBbmc/REDCapR/blob/master/R/redcap_read_oneshot.R).
 
 If this fails, consider attempting again with the uri and token used above in the Postman example.
 
 This check avoids SSL in order to simplify the troubleshooting.  SSL is supported by default in the [PyCap](http://sburns.org/PyCap/) and [REDCapR](https://github.com/OuhscBbmc/REDCapR) packages.
    
  ``` r
    redcap_uri <- "https://the.urlofyourinsitution.edu/api/" #Adapt this to your server.
    token <- "your-secret-token" #Adapt this to your user's token.
    records_collapsed <- "1,2,3" #This assumes that their dataset contains ID values of 1, 2, and 3.  Adapt this to their dataset.
    fields_collapsed <- "record_id,first_name,last_name" #This assumes that their dataset contains variables called 'recordid', 'first_name', and 'last_name'.  Adapt this to their dataset.
    
    raw_csv <- RCurl::postForm(
      uri = redcap_uri
      , token = token
      , content = 'record'
      , format = 'csv'
      , type = 'flat'
      , rawOrLabel = 'raw'
      , exportDataAccessGroups = 'true'
      , records = records_collapsed
      , fields = fields_collapsed
      , .opts = RCurl::curlOptions(ssl.verifypeer = FALSE)
    )
  ``` 
 1. **Can the user query a *entire* REDCap project using RCurl?**   
 There are two advantages of trying a subset of the data.  First, small datasets avoid the time-out errors that plague large datasets.  Second, it may avoid problematic values being passed through the pipeline.  If the current check fails but the previous check succeedes, then experiment with different expanses of records and fields.  This should help determine which values are causing the problems, or if there's simply too much data being pulled in one pass.  
 
 If the desired dataset is too large, consider if you can prune unnecessary records or fields.  If not, one solution is to pull smaller, multiple batches using the API, then reassemble them.  The `redcap_read()` function in [REDCapR](https://github.com/OuhscBbmc/REDCapR/blob/master/DocumentationPeek.pdf) does this automatically, and allows the user to specify a `batch_size`.
 
  ``` r
    redcap_uri <- "https://the.urlofyourinsitution.edu/api/" #Adapt this to your server.
    token <- "your-secret-token" #Adapt this to your user's token.
    records_collapsed <- NULL
    fields_collapsed <- NULL
    
    raw_csv <- RCurl::postForm(
      uri = redcap_uri
      , token = token
      , content = 'record'
      , format = 'csv'
      , type = 'flat'
      , rawOrLabel = 'raw'
      , exportDataAccessGroups = 'true'
      , records = records_collapsed
      , fields = fields_collapsed
      , .opts = RCurl::curlOptions(ssl.verifypeer = FALSE)
    )
  ```
   
## Calling API from REDCapR
 [REDCapR](https://github.com/OuhscBbmc/REDCapR) is a package that uses [RCurl](http://cran.r-project.org/web/packages/RCurl) to communicate with REDCap, and wraps convience functions around it to reduce the size and complexity of the user's code.  The package's basic functions are demonstrated in [this vignette](http://htmlpreview.github.io/?https://github.com/OuhscBbmc/REDCapR/blob/master/inst/doc/BasicREDCapROperations.html) and are documented in its [reference manual](https://github.com/OuhscBbmc/REDCapR/blob/master/DocumentationPeek.pdf) (click the 'View Raw' link).
 
 1. **Is REDCapR installed on the user's machine?**   
 Currently the easiest way to install REDCapR is with the [devtools](https://github.com/hadley/devtools).  The follow code installs devtools, then installs REDCapR.
  ``` r
  install.packages("devtools")
  devtools::install_github(repo="OuhscBbmc/REDCapR")
  ```
   
 1. **Does REDCapR load successfully on the user's machine?**    
 If so, running `require(REDCapR)` should produce the following output if you're starting with a fresh session of R:
  ```
  > require(REDCapR)
  Loading required package: REDCapR
  ```
     
 1. **Can the user export from an example project?**    
 This is the same fake data hosted by the [OUHSC BBMC](http://ouhsc.edu/bbmc/) as in the previous section.
  ``` r
  library(REDCapR) #Load the package into the current R session.
  uri <- "<copy this value from the Postman screenshot above>"
  token <- "9A81268476645C4E5F03428B8AC3AA7B"
  redcap_read(redcap_uri=uri, token=token)$data
  ```
  
  The previous code should produce output similar to this.  Notice there are five rows and the columns will wrap around, depending on the width of your console window.
  ``` r
5 records and 1 columns were read from REDCap in 0.41 seconds.
Starting to read 5 records  at 2014-06-27 17:19:49
Reading batch 1 of 1, with ids 1 through 5.
5 records and 16 columns were read from REDCap in 0.42 seconds.

  record_id first_name last_name                                 address      telephone               email
1         1     Nutmeg  Nutmouse 14 Rose Cottage St.\nKenning UK, 323232 (432) 456-4848     nutty@mouse.com
2         2     Tumtum  Nutmouse 14 Rose Cottage Blvd.\nKenning UK 34243 (234) 234-2343    tummy@mouse.comm
3         3     Marcus      Wood          243 Hill St.\nGuthrie OK 73402 (433) 435-9865        mw@mwood.net
4         4      Trudy       DAG          342 Elm\nDuncanville TX, 75116 (987) 654-3210 peroxide@blonde.com
5         5   John Lee    Walker      Hotel Suite\nNew Orleans LA, 70115 (333) 333-4444  left@hippocket.com

         dob age ethnicity race sex height weight   bmi
1 2003-08-30  10         1    2   0   5.00      1 400.0
2 2003-03-10  10         1    6   1   6.00      1 277.8
3 1934-04-09  79         0    4   1 180.00     80  24.7
4 1952-11-02  61         1    4   0 165.00     54  19.8
5 1955-04-15  58         1    4   1 193.04    104  27.9

                                                                                                     comments
1                                                                     Character in a book, with some guessing
2                                                                          A mouse character from a good book
3                                                                                          completely made up
4 This record doesn't have a DAG assigned\n\nSo call up Trudy on the telephone\nSend her a letter in the mail
5                 Had a hand for trouble and a eye for cash\n\nHe had a gold watch chain and a black mustache

  demographics_complete
1                     2
2                     2
3                     2
4                     2
5                     2
  ```
      
 1. **Can the user *export* from their own project?**    
 The code is similar to the previous check, but the `uri` and `token` values will need to be modified.
  ``` r
  library(REDCapR) #Load the package into the current R session, if you haven't already.
  redcap_uri <- "https://the.urlofyourinsitution.edu/api/" #Adapt this to your server.
  token <- "your-secret-token" #Adapt this to your user's token.
  redcap_read(redcap_uri=uri, token=token)
  ```
      
 1. **Can the user *import* to their own project?**    
 Writing records can be trickier, because the schema (eg, the names and data types) must match the project.  This section will be expanded in the future.  Current recommendations include checking if you can write to simpler projects (perhaps with 1 ID field and 1 string field), and progressively moving to mimic the problematic project's schema and dataset.
 
 1. **Is the operation still unsuccessful using REDCapR?**    
 If so the "Can the user query a *entire* REDCap project using RCurl?" check succeeded, but the REDCapR checks did not, consider posting a new [GitHub issue](https://github.com/OuhscBbmc/REDCapR/issues) to the package developers.
 
 
## Document Info
This document is primarily based on REDCap version 5.11.3, and was last updated 2016-06-27.  A development version of the document is available on GitHub: http://htmlpreview.github.io/?https://github.com/OuhscBbmc/REDCapR/blob/RefClassTry/inst/doc/TroubleshootingApiCalls.html.