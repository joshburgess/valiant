UPDATE posts
SET published_at = now()
WHERE id = $1 AND published_at IS NULL
