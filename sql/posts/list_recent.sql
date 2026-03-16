SELECT id, title, published_at
FROM posts
WHERE published_at IS NOT NULL
ORDER BY published_at DESC
LIMIT $1
