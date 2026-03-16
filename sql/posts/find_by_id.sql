SELECT id, author_id, title, body, published_at, created_at
FROM posts
WHERE id = $1
