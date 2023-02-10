# grannies-coockie

Backend for simple news site, powered by warp, persistent, servant.
Features data streaming, hashing with salt, basic authentication, pagination by key.

TODO:
1) GET News returns pictures's URL - Done
2) Streaming - Done
3) Get rid off hardcode connection string and port - Done
4) OpenID - some day later
5) Pictures get headers - fixed
6) Write tests
7) Make documentation
8) Maybe add logging
9) Remove prefixes in persistent
10) Check error codes



API description draft:
  GET /users/ - get list of all users, paginated by key
  GET /users/:login - get speciefic user by login
  POST /users/ - post user
  
  
  POST /news/ - requires basic auth 
  GET /news/ - get list of news with picture's URL's
  
     can be filtered by: 
         1) creation date (createdAt,createdBefore,createdAfter),
         2) author
         3) category
         
     can be sorted by 
        1) date
        2) author
        3) category
    
    Making sorted requests
     since server uses pagination by key instead of offset, 
     for sorted queries "offset-key" parameter expects both sorted and primary key ( login ) separated with ":"
     like this:  offset-key = "<userLogin>:<join_date>"
     
     sorting parameter should be specified with "sort_by" header key and one of values: [createdAt, author, category]


  GET /images/:imageID - get single image by id
  GET /images_by_newsID - get all news related to specified news
  POST /images/ - post image, body in form-data, with news_id in body, admins only
  
  
  POST /category/ - post a category, admins only
  GET /category/ - get a category