---
title: newexpjs
---
<SwmSnippet path="newjexps.js" line="1" collapsed>

---

# inserting Elements.

app.post()

/

```
const express = require('express');
const app = express();
const port = process.env.PORT || 3000;

// Middleware to parse JSON bodies
app.use(express.json());

// In-memory storage for posts
let posts = [];

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

app.listen(port, () => {
```

---

</SwmSnippet>

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBc3dpbW0lM0ElM0FHb3d0aGFta2FyMTIz" repo-name="swimm"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
