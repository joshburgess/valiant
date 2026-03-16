SELECT id, name, email, is_active, created_at
FROM users
WHERE id = $1
