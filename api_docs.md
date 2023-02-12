## GET /audio/download/:audio_id

### Captures:

- *audio_id*: Audio file ID

### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/ogg`

- No response body

## GET /audio/play/:audio_id

### Captures:

- *audio_id*: Audio file ID

### Headers:

- This endpoint is sensitive to the value of the **range** HTTP header.

### Response:

- Status code 206
- Headers: [("accept-ranges","Sample Text"),("content-length","123"),("content-range","Sample Text"),("content-type","Sample Text")]

- Supported content types are:

    - `application/octet-stream`

- Response body as below.

```

```

## POST /audio/upload/big/:audio_id

### Captures:

- *audio_id*: Audio file ID

### Headers:

- This endpoint is sensitive to the value of the **Authorization** HTTP header.

### Request:

- Supported content types are:

    - `application/octet-stream`

### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- Example (`application/json;charset=utf-8`, `application/json`):

```javascript
false
```

- Example (`application/json;charset=utf-8`, `application/json`):

```javascript
true
```

## POST /audio/upload/small/:audio_id

### Multipart Request Samples

This endpoint takes `multipart/form-data` requests. The following is a list of sample requests:



### Captures:

- *audio_id*: Audio file ID

### Headers:

- This endpoint is sensitive to the value of the **Authorization** HTTP header.

### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- Example (`application/json;charset=utf-8`, `application/json`):

```javascript
false
```

- Example (`application/json;charset=utf-8`, `application/json`):

```javascript
true
```

## POST /category

### Headers:

- This endpoint is sensitive to the value of the **Authorization** HTTP header.

### Request:

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- Example (`application/json;charset=utf-8`, `application/json`):

```javascript
{"name":"olympic games","parent":"sport"}
```

### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- Example (`application/json;charset=utf-8`, `application/json`):

```javascript
false
```

- Example (`application/json;charset=utf-8`, `application/json`):

```javascript
true
```

## GET /category/:category_id

### Captures:

- *category_id*: Category ID

### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- Example (`application/json;charset=utf-8`, `application/json`):

```javascript
{"name":"olympic games","parent":"sport"}
```

## POST /images

### Multipart Request Samples

This endpoint takes `multipart/form-data` requests. The following is a list of sample requests:



### Headers:

- This endpoint is sensitive to the value of the **Authorization** HTTP header.

### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- Example (`application/json;charset=utf-8`, `application/json`):

```javascript
false
```

- Example (`application/json;charset=utf-8`, `application/json`):

```javascript
true
```

## GET /images/:image_id

### Captures:

- *image_id*: Image ID

### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `image/jpeg`

- Response body as below.

```

```

## GET /images_by_newsID/:news_id

### Captures:

- *news_id*: News ID

### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- Example (`application/json;charset=utf-8`, `application/json`):

```javascript
[]
```

- Example (`application/json;charset=utf-8`, `application/json`):

```javascript
[{"content":"","image_id":123,"news_id":123}]
```

- Example (`application/json;charset=utf-8`):

```javascript
[{"content":"","image_id":123,"news_id":123},{"content":"","image_id":123,"news_id":123}]
```

## GET /news

### Headers:

- This endpoint is sensitive to the value of the **limit** HTTP header.
- This endpoint is sensitive to the value of the **offset-key** HTTP header.

### GET Parameters:

- createdAt
     - **Values**: *16.01.1998*
     - **Description**: Date when news was created

- createdBefore
     - **Values**: *16.01.1998*
     - **Description**: Latest date when news was created

- createdAfter
     - **Values**: *16.01.1998*
     - **Description**: Minimal date when news was created

- author
     - **Values**: *Stephen King, Thomas Mann, Hermann Hesse*
     - **Description**: List of news authors
     - This parameter is a **list**. All GET parameters with the name author[] will forward their values in a list to the handler.

- category
     - **Values**: *news, sport, politics*
     - **Description**: List of news categories
     - This parameter is a **list**. All GET parameters with the name category[] will forward their values in a list to the handler.

- sort_by
     - **Values**: *createdAt, author, category*
     - **Description**: Result will be sorted by specified field in ascending order. Notice that secondary sorting field is always userLogin in ascending order, and if sort_by missing, its primary sorting field


### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- Example (`application/json;charset=utf-8`, `application/json`):

```javascript
[]
```

- Example (`application/json;charset=utf-8`, `application/json`):

```javascript
[{"author":"bearmond","category":"sport","content":"We got a new record","creation_date":"7329-04-05","header":"New jumping record","imagesURLs":["localhost:3000/images/123"],"is_published":false,"newsId":123}]
```

- Example (`application/json;charset=utf-8`):

```javascript
[{"author":"bearmond","category":"sport","content":"We got a new record","creation_date":"7329-04-05","header":"New jumping record","imagesURLs":["localhost:3000/images/123"],"is_published":false,"newsId":123},{"author":"bearmond","category":"sport","content":"We got a new record","creation_date":"7329-04-05","header":"New jumping record","imagesURLs":["localhost:3000/images/123"],"is_published":false,"newsId":123}]
```

## POST /news

### Headers:

- This endpoint is sensitive to the value of the **Authorization** HTTP header.

### Request:

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- Example (`application/json;charset=utf-8`, `application/json`):

```javascript
{"author":"bearmond","category":"sport","content":"We got a new record","creation_date":"7329-04-05","header":"New jumping record","is_published":false,"newsId":123}
```

### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- Example (`application/json;charset=utf-8`, `application/json`):

```javascript
false
```

- Example (`application/json;charset=utf-8`, `application/json`):

```javascript
true
```

## GET /users

### Headers:

- This endpoint is sensitive to the value of the **limit** HTTP header.
- This endpoint is sensitive to the value of the **offset-key** HTTP header.

### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- Example (`application/json;charset=utf-8`, `application/json`):

```javascript
[]
```

- Example (`application/json;charset=utf-8`, `application/json`):

```javascript
[{"admin":false,"author":false,"email":"hacker@gmail.com","join_date":"7329-04-05","login":"hacker228","pass":"secretPass","phone_number":"123456"}]
```

- Example (`application/json;charset=utf-8`):

```javascript
[{"admin":false,"author":false,"email":"hacker@gmail.com","join_date":"7329-04-05","login":"hacker228","pass":"secretPass","phone_number":"123456"},{"admin":false,"author":false,"email":"hacker@gmail.com","join_date":"7329-04-05","login":"hacker228","pass":"secretPass","phone_number":"123456"}]
```

## POST /users

### Request:

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- Example (`application/json;charset=utf-8`, `application/json`):

```javascript
{"admin":false,"author":false,"email":"hacker@gmail.com","join_date":"7329-04-05","login":"hacker228","pass":"secretPass","phone_number":"123456"}
```

### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- Example (`application/json;charset=utf-8`, `application/json`):

```javascript
false
```

- Example (`application/json;charset=utf-8`, `application/json`):

```javascript
true
```

## GET /users/:login

### Captures:

- *login*: User login

### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- Example (`application/json;charset=utf-8`, `application/json`):

```javascript
{"admin":false,"author":false,"email":"hacker@gmail.com","join_date":"7329-04-05","login":"hacker228","pass":"secretPass","phone_number":"123456"}
```

## PATCH /users/grant_admin/:login

### Captures:

- *login*: User login

### Headers:

- This endpoint is sensitive to the value of the **Authorization** HTTP header.

### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- Example (`application/json;charset=utf-8`, `application/json`):

```javascript
false
```

- Example (`application/json;charset=utf-8`, `application/json`):

```javascript
true
```

## PATCH /users/grant_author/:login

### Captures:

- *login*: User login

### Headers:

- This endpoint is sensitive to the value of the **Authorization** HTTP header.

### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- Example (`application/json;charset=utf-8`, `application/json`):

```javascript
false
```

- Example (`application/json;charset=utf-8`, `application/json`):

```javascript
true
```

