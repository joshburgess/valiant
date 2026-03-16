SELECT p.id, p.title, p.body, p.published_at, u.name as author_name
FROM posts p
JOIN users u ON u.id = p.author_id
WHERE p.id = $1
