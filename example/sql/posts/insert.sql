INSERT INTO posts (author_id, title, body)
VALUES ($1, $2, $3)
RETURNING id
