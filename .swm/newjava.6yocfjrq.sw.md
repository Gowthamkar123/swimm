---
title: newjava
---
# Introduction

This document will walk you through the implementation of a simple Express.js application for managing posts. The application provides basic CRUD operations using in-memory storage.

We will cover:

1. How posts are stored and retrieved.
2. How new posts are created.
3. How the server is initialized and listens for requests.

# Storing and retrieving posts

<SwmSnippet path="/newjexps.js" line="10">

---

The application uses an in-memory array to store posts. This is a straightforward approach for temporary data storage, suitable for development and testing purposes. The GET endpoints allow retrieval of all posts or a specific post by ID, ensuring that users can access the data they need.

```
// GET all posts
app.get('/posts', (req, res) => {
    res.json(posts);
});

// GET post by id
app.get('/posts/:id', (req, res) => {
    const post = posts.find(p => p.id === parseInt(req.params.id));
    if (!post) return res.status(404).json({ message: 'Post not found' });
    res.json(post);
});
```

---

</SwmSnippet>

# Creating new posts

<SwmSnippet path="/newjexps.js" line="22">

---

The application provides a POST endpoint to create new posts. Each post is assigned a unique ID based on the current length of the posts array. This method is simple and effective for generating IDs in a non-persistent storage scenario. The endpoint responds with the newly created post, confirming successful creation.

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

# Server initialization

<SwmSnippet path="/newjexps.js" line="34">

---

The server is initialized to listen on a specified port, which defaults to 3000 if not set in the environment variables. This setup ensures the application is ready to handle incoming requests and provides feedback via console logging.

```
app.listen(port, () => {
    console.log(`Server running on port ${port}`);
});
```

---

</SwmSnippet>

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBc3dpbW0lM0ElM0FHb3d0aGFta2FyMTIz" repo-name="swimm"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
