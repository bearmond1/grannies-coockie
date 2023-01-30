# grannies-coockie

TODO:
1) GET News returns pictures's URL 
2) Google account inegration
3) Find where to put streaming
4) Write OpenAPI documentation
5) Maybe add logging
6) Remove prefixes



Backend for simple news site, powered by warp, persistent, servant.
Features hashing with salt, basic authentication, pagination by key.

API:
  GET /users/ - get list of all users, paginated by key
  GET /users/:login - get speciefic user by login
  
  GET /news/ - get list of news
  
     can be filtered by: 
         1) creation date (createdAt,createdBefore,createdAfter),
         2) author
         3) category
         
     can be sorted by 
        1) date
        2) author
        3) category
        4) number of pictures
    
    Making sorted requests
     since server uses pagination by key instead of offset, 
     for sorted queries "offset-key" parameter expects both sorted and primary key ( login ) separated with ":"
     like this:  offset-key = "<userLogin>:<join_date>"
     
     sorting parameter should be specified with "sort_by" header key and one of values: [createdAt,
     
