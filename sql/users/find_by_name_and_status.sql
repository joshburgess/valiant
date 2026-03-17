SELECT id, name, email
FROM users
WHERE name = :name
  AND is_active = :active
