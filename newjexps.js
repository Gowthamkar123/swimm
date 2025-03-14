const express = require('express');
const app = express();
const port = process.env.PORT || 3000;



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
    console.log(`Server running on port ${port}`);
});
