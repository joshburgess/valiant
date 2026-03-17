# High-Performance Database Libraries in Haskell

In the Haskell ecosystem, performance usually correlates with how thin the abstraction layer is over the database's native protocol. While frameworks like **Persistent** or **Beam** focus on developer ergonomics and backend-agnosticism, **Hasql** is designed specifically to eliminate "performance taxes."

---

## The Performance Leader: Hasql

**Hasql** is widely regarded as the fastest PostgreSQL driver in Haskell. It achieves its lead by bypassing standard overheads in three specific ways:

* **Binary Protocol:** Unlike most libraries (including `postgresql-simple`), which use the text-based protocol, Hasql uses the **PostgreSQL binary protocol**. This eliminates CPU cycles spent serializing data to text and parsing it back on both ends.
* **Statement Caching:** It utilizes prepared statements by default, allowing the database to skip query planning for repeat queries.
* **Minimalist Abstraction:** It avoids intermediate "sum type" wrappers (like Persistent's `PersistValue`) that add allocation and pattern-matching overhead during decoding.



---

## Comparison of Popular Options

| Library | Primary Focus | Performance Level | Best For |
| :--- | :--- | :--- | :--- |
| **Hasql** | Speed & Control | **Extreme** | High-throughput services (e.g., used by **PostgREST**). |
| **Rel8** | Type Safety (built on Hasql) | **High** | High speed combined with a powerful, type-safe EDSL. |
| **Opaleye** | Correctness | **Moderate/High** | Complex analytical queries where SQL correctness is the priority. |
| **Beam** | Backend-Agnostic | **Moderate** | Projects needing to switch between Postgres and SQLite. |
| **Persistent** | Ease of Use (ORM-like) | **Standard** | Rapid prototyping where DB latency isn't the primary bottleneck. |

---

## Performance Considerations Beyond the Driver

If you are optimizing a Haskell application, the library choice is only one part of the equation. Two other factors often have a larger impact on real-world latency:

### 1. The N+1 Problem (Batching)
Haskell excels at solving the N+1 query problem using **Applicative Functors**.
* **Haxl:** Developed by Facebook, this is a framework for **efficiently batching** and concurrentizing data fetches. 
* **Fetch:** A similar, community-maintained library for data fetching.
> Wrapping your database calls in a batching layer often yields a greater speedup than switching drivers by reducing network round trips.

### 2. Serialization
For non-SQL data storage or caching, the choice of serialization library is critical.
* **Store** or **Serialise (CBOR):** These provide significantly faster binary serialization than the standard **Aeson (JSON)** for data-heavy applications.
