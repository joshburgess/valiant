SELECT p.id, p.title, u.name as author_name, p.published_at
FROM posts p
JOIN users u ON u.id = p.author_id
WHERE p.published_at IS NOT NULL
ORDER BY p.published_at DESC
LIMIT $1
