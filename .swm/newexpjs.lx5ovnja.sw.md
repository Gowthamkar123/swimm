---
title: newexpjs
---
# Introduction

This document will walk you through the implementation of a basic Express.js application for managing posts. The purpose of this application is to provide a simple API for creating, retrieving, and managing posts stored in-memory.

We will cover:

1. How the application initializes and sets up the server.
2. How the application handles retrieving posts.
3. How the application handles creating new posts.

# Application initialization

<SwmSnippet path="/newjexps.js" line="35">

---

The application is initialized using Express.js. We set up the server to listen on a specified port, which defaults to 3000 if not provided by the environment. This setup is crucial for starting the server and making it ready to handle incoming requests.

```
app.listen(port, () => {
    console.log(`Server running on port ${port}`);
});
```

---

</SwmSnippet>

# Retrieving posts

<SwmSnippet path="/newjexps.js" line="11">

---

The application provides endpoints to retrieve all posts or a specific post by its ID. This is important for accessing the stored data and ensuring users can view the posts they are interested in.

```

```

---

</SwmSnippet>

# Creating new posts

<SwmSnippet path="/newjexps.js" line="23">

---

The application allows users to create new posts by sending a POST request. Each post is assigned a unique ID and timestamp upon creation. This functionality is essential for adding new content to the in-memory storage.

```
// CREATE new post
app.post('/posts', (req, res) => {
    const post = {
        id: posts.length + 1,
        title: req.body.title,
        content: req.body.content,
        createdAt: new Date()
    };
    posts.push(post);
    res.status(201).json(post);
});
```

---

</SwmSnippet>

By structuring the application in this way, we ensure that it can handle basic CRUD operations for posts, making it a simple yet effective tool for managing post data.

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBc3dpbW0lM0ElM0FHb3d0aGFta2FyMTIz" repo-name="swimm"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
