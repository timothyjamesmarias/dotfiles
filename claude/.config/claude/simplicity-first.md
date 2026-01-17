# Simplicity-First Principles

## Core Philosophy

**Choose the simplest solution that solves the problem.** Complexity should only be introduced when it provides clear, measurable value.

## YAGNI (You Aren't Gonna Need It)

- **Build for today's requirements, not tomorrow's possibilities**
- Don't add features "just in case" - wait until they're actually needed
- Don't build abstractions until you have 2-3 concrete use cases
- Resist the urge to make things "more flexible" without a concrete reason

```javascript
// Bad - premature abstraction
class DataFetcher {
  constructor(strategy, cache, retry, logger, metrics) { ... }
}

// Good - solve the actual problem
function fetchUserData(userId) {
  return fetch(`/api/users/${userId}`).then(r => r.json());
}
```

## Prefer Simple over Clever

- **Boring code is good code** - it's easy to understand and maintain
- Avoid language tricks or obscure features unless they significantly improve clarity
- If you need to explain how the code works, it's probably too clever
- Choose explicit over implicit when it improves understanding

```javascript
// Clever but hard to parse
const active = users.filter(u => u.status === 1).map(u => u.id);

// Simple and clear
const activeUserIds = users
  .filter(user => user.isActive)
  .map(user => user.id);
```

## Start with the Straightforward Solution

1. **First: Write the naive, obvious solution**
2. **Then: Only optimize if there's a proven problem**
3. **Measure before optimizing** - don't guess at performance bottlenecks

```javascript
// Start here - simple and works
function findUser(users, id) {
  return users.find(user => user.id === id);
}

// Only move to this if you've measured and proven the above is too slow
function findUser(users, id) {
  const userMap = new Map(users.map(u => [u.id, u]));
  return userMap.get(id);
}
```

## Avoid Over-Engineering

- **Don't build frameworks** - solve specific problems
- Question every layer of abstraction: "What would happen if I didn't have this?"
- Favor composition over complex inheritance hierarchies
- Use design patterns only when they clearly simplify the solution

## Choose Standard over Custom

- **Use language/framework built-ins** before reaching for libraries
- Use well-established libraries before writing your own
- Follow platform conventions rather than inventing new patterns
- "Not invented here" is not a valid reason to build something custom

```javascript
// Over-engineered custom solution
class DateFormatter {
  format(date, pattern) { /* custom parsing logic */ }
}

// Use the platform
date.toISOString()
date.toLocaleDateString()
new Intl.DateTimeFormat('en-US').format(date)
```

## Flat is Better than Nested

- **Prefer linear code over deeply nested structures**
- Use early returns instead of nested if-statements
- Extract complex conditions into well-named variables or functions
- Keep control flow easy to trace from top to bottom

## Delete Code

- **The best code is no code at all**
- Before adding a feature, see if existing functionality can be repurposed
- Regularly remove unused code, commented code, and dead features
- Favor deleting over refactoring when both are options

## Concrete over Abstract

- **Start concrete, abstract only when patterns emerge**
- Don't create generic solutions for a single use case
- Wait until you have 2-3 similar implementations before abstracting
- It's easier to extract abstractions later than to fix wrong abstractions

## Read the Error Message

- **The error often tells you exactly what's wrong**
- Before diving into complex debugging, read the full error message
- Check the obvious things first: typos, missing imports, wrong types
- Use the debugger before adding complex logging

## Question Complexity

Before adding complexity, ask:
- **What problem does this solve?**
- **What's the simplest way to solve this problem?**
- **What's the cost of not solving this?**
- **Can I solve this with less code?**
- **Will I understand this code in 6 months?**
- **Can someone else understand this code quickly?**

## Examples

### Over-complicated dependency injection
```javascript
// Bad - over-engineered
class Container {
  register(name, factory) { ... }
  resolve(name) { ... }
  singleton(name, factory) { ... }
}

const container = new Container();
container.register('userService', () => new UserService(container.resolve('db')));
const userService = container.resolve('userService');

// Good - just pass dependencies
const db = createDatabase();
const userService = new UserService(db);
```

### Unnecessary abstraction layers
```javascript
// Bad - abstraction for one use case
class UserRepository {
  findById(id) { return this.db.users.findOne({ id }); }
  findAll() { return this.db.users.find(); }
  create(data) { return this.db.users.insert(data); }
}

// Good - direct and clear
const user = await db.users.findOne({ id });
const users = await db.users.find();
const newUser = await db.users.insert(userData);
```

### Premature optimization
```javascript
// Bad - optimizing before measuring
class OptimizedUserList {
  constructor(users) {
    this.userMap = new Map();
    this.emailIndex = new Map();
    this.nameIndex = new Map();
    // ... complex indexing logic
  }
}

// Good - simple until proven slow
const users = [/* user objects */];
const user = users.find(u => u.id === targetId);
```

## Remember

- **Complexity is the enemy of reliability**
- **Simple code is easier to test, debug, and modify**
- **Future you will thank present you for choosing simplicity**
- **When in doubt, do the simplest thing that could possibly work**
