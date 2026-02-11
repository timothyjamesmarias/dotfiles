## Code Generation and Style Rules

**IMPORTANT: The following rules MUST be followed for all code generation, refactoring, and development work.**

### Core Principles

Key points:
- **Choose the simplest solution that solves the problem**
- **YAGNI**: Build for today's requirements, not tomorrow's possibilities
- **Prefer simple over clever** - boring code is good code
- **Start with the straightforward solution**, optimize only when proven necessary
- **Question complexity** at every step
- **Delete code** rather than commenting it out

### Code Quality Requirements

Every piece of code MUST:
- Be immediately understandable
- Follow existing project conventions
- Handle errors explicitly (no silent failures)
- Be testable
- Have a single, clear purpose

**Forbidden patterns:**
- Magic numbers or strings (use named constants)
- Deep nesting (max 2-3 levels)
- Large methods (if it doesn't fit on one screen, break it down)
- Boolean parameters (create separate methods instead)
- Commented-out code (delete it)
- Clever tricks that sacrifice clarity

### Code Style Guidelines

**Method Design:**
- Keep methods small and focused
- Methods should be easy to parse and test
- Avoid large methods with many conditionals

**Conditional Logic:**
- Prefer early returns over else statements
- Avoid nested conditionals
- Name extracted methods descriptively

**Function Parameters:**
- Limit to 3 or fewer parameters
- Use objects for multiple related parameters
- Avoid boolean parameters

**Nesting:**
- Maximum depth of 2-3 levels
- Extract nested blocks into functions
- Use early returns to reduce nesting

### Naming Conventions

**Variables and Functions:**
- camelCase for Kotlin/Java/JS/TS
- Boolean variables: `isActive`, `hasPermission`, `canEdit`
- Functions should be verbs: `calculateTotal()`, `formatDate()`

**Classes and Types:**
- PascalCase for class and type names
- Avoid interface prefixes (use `UserService`, not `IUserService`)

**Constants:**
- SCREAMING_SNAKE_CASE for true constants
- Group related constants in enums or objects

### Security Considerations

Always check for:
- Input validation (XSS, SQL injection, command injection)
- Authentication and authorization
- Sensitive data exposure
- Resource exhaustion
- Race conditions in concurrent code

### Review Checklist

Before marking work as done, verify:
- [ ] Does this solve the actual problem?
- [ ] Is this the simplest solution?
- [ ] Are all edge cases handled?
- [ ] Would I understand this code in 6 months?
- [ ] Would someone else understand this code quickly?
- [ ] Are there any security concerns?
- [ ] Do all tests pass?
- [ ] Is error handling explicit and appropriate?

---

# Code Generation Rules

These are strict rules to follow when generating code. Quality and correctness over speed.

## Before Writing Any Code

1. **Understand the requirement fully** - Ask questions if anything is unclear
2. **Check existing patterns** - Look at how similar problems are solved in the codebase
3. **Choose the simplest approach** - Reference simplicity-first.md principles
4. **Consider edge cases** - Don't just solve the happy path

## Code Quality Requirements

### Every piece of code must:

- **Be immediately understandable** - If it requires explanation, it's too complex
- **Follow existing project conventions** - Match the style of surrounding code
- **Handle errors explicitly** - No silent failures or ignored edge cases
- **Be testable** - If it's hard to test, redesign it
- **Have a single, clear purpose** - Methods do one thing well

### Forbidden patterns:

- Magic numbers or strings (use named constants)
- Deep nesting (max 2-3 levels)
- Large methods (if it doesn't fit on one screen, break it down)
- Boolean parameters (create separate methods instead)
- Commented-out code (delete it)
- Clever tricks that sacrifice clarity

## Type Safety and Validation

- Use the type system to prevent errors (TypeScript, Rust, Kotlin types)
- Validate inputs at boundaries (API endpoints, public methods)
- Make illegal states unrepresentable with types
- Prefer compile-time checks over runtime checks

## Documentation Requirements

**When to add comments:**

- Complex business logic that isn't obvious from code
- Non-obvious performance considerations
- Edge cases being handled
- Reasons for choosing one approach over another

**Don't comment:**

- What the code does (code should show this)
- Obvious things
- To explain bad code (refactor instead)

## Testing Expectations

When implementing a feature:

1. **Existing tests must pass** - Never break working functionality
2. **Add tests for new code** - Test the behavior, not the implementation
3. **Test edge cases** - Null/undefined, empty collections, boundary values
4. **Use meaningful test names** - Describe what behavior is being tested

## Security Considerations

Always check for:

- Input validation (XSS, SQL injection, command injection)
- Authentication and authorization
- Sensitive data exposure
- Resource exhaustion (infinite loops, unbounded growth)
- Race conditions in concurrent code

## Review Before Completing

Before marking work as done, verify:

- [ ] Does this solve the actual problem?
- [ ] Is this the simplest solution?
- [ ] Are all edge cases handled?
- [ ] Would I understand this code in 6 months?
- [ ] Would someone else understand this code quickly?
- [ ] Are there any security concerns?
- [ ] Do all tests pass?
- [ ] Is error handling explicit and appropriate?

## When Uncertain

- **Ask questions** rather than making assumptions
- **Propose multiple approaches** if there are trade-offs
- **Explain trade-offs clearly** so informed decisions can be made
- **Start with the simplest option** unless there's a clear reason not to

# Code Style Guide

## Method Design

- **Keep methods small and focused**: Each method should have a single, clear responsibility
- **Methods should be easy to parse**: If a method is difficult to understand at a glance, break it down
- **Methods should be easy to test**: Single-responsibility methods are naturally more testable
- **Avoid large methods with many conditionals**: Break complex logic into smaller, named helper methods

## Conditional Logic

- **Prefer early returns over else statements**: Use guard clauses to handle edge cases first
- **Avoid nested conditionals**: Flatten logic with early returns or extract to separate methods
- **Name extracted methods descriptively**: Method names should clearly indicate what condition they check or what they do

## Naming Conventions

- **Use descriptive, searchable names**: `getUsersByRegistrationDate()` not `getUsers2()`
- **Avoid abbreviations**: `customer` not `cust`, `message` not `msg`
- **Boolean names should be questions**: `isActive`, `hasPermission`, `canEdit`
- **Functions should be verbs**: `calculateTotal()`, `formatDate()`, `validateInput()`

## Function Parameters

- **Limit parameters to 3 or fewer**: More than 3 suggests the function is doing too much
- **Use objects for multiple related parameters**: Instead of `createUser(name, email, age, address)`, use `createUser({ name, email, age, address })`
- **Avoid boolean parameters**: They hide what the function does. Instead of `save(true)`, create `saveAndValidate()` and `saveWithoutValidation()`

## Magic Values

- **No magic numbers or strings**: Use named constants
- **Extract constants with descriptive names**: `const MAX_RETRY_ATTEMPTS = 3` not just `3`
- **Group related constants**: Use enums or constant objects for related values

```javascript
// Bad
if (status === 2) {
  return 'Active';
}

// Good
const STATUS_ACTIVE = 2;
if (status === STATUS_ACTIVE) {
  return 'Active';
}

// Even better - use enums/objects
const UserStatus = {
  INACTIVE: 1,
  ACTIVE: 2,
  SUSPENDED: 3,
};
if (status === UserStatus.ACTIVE) {
  return 'Active';
}
```

## Comments

- **Code should be self-documenting**: Prefer clear naming over comments
- **Comments explain WHY, not WHAT**: The code shows what it does, comments explain business logic or decisions
- **Document complex algorithms or non-obvious behavior**
- **Remove commented-out code**: Use version control instead

```javascript
// Bad - comment explains what (code already shows this)
// Loop through users
users.forEach(user => { ... });

// Good - comment explains why
// We process users in batches to avoid overwhelming the email service
users.forEach(user => { ... });
```

## Nesting

- **Maximum nesting depth of 2-3 levels**: Deep nesting is hard to follow
- **Extract nested blocks into functions**: Each level of abstraction becomes a named function
- **Use early returns to reduce nesting**

## DRY (Don't Repeat Yourself)

- **Extract repeated logic into functions**
- **But don't over-abstract**: Two similar things that change for different reasons should stay separate
- **Three strikes rule**: If you write the same code three times, extract it into a function

## Language-Specific Guidelines

### Ruby

- Follow community Ruby style guide (Rubocop defaults)
- Use snake_case for methods and variables
- Use symbols (`:symbol`) for keys when appropriate
- Prefer `do...end` for multi-line blocks, `{ }` for single-line
- Use safe navigation operator (`&.`) to avoid nil checks
- Prefer `each` over `for` loops

### PHP

- Follow PSR-12 coding standard
- Use snake_case for functions and variables
- Type hint all function parameters and return types (PHP 7.4+)
- Use strict types: `declare(strict_types=1);`
- Prefer early returns to reduce nesting
- Use null coalescing operator (`??`) for default values

## Examples

### Bad - Large method with nested conditionals

```javascript
function processUser(user) {
  if (user) {
    if (user.active) {
      if (user.email) {
        return sendEmail(user.email);
      } else {
        return logError('No email');
      }
    } else {
      return logError('User inactive');
    }
  } else {
    return logError('No user');
  }
}
```

### Good - Early returns and extracted methods

```javascript
function processUser(user) {
  if (!user) return logError('No user');
  if (!user.active) return logError('User inactive');
  if (!user.email) return logError('No email');

  return sendEmail(user.email);
}
```

### Bad - Method doing too much

```javascript
function handleFormSubmit(formData) {
  // validate
  if (!formData.name) return error;
  if (!formData.email) return error;

  // sanitize
  const cleanName = formData.name.trim();
  const cleanEmail = formData.email.toLowerCase();

  // save to database
  const user = db.users.create({ name: cleanName, email: cleanEmail });

  // send email
  emailService.sendWelcome(user.email);

  // log analytics
  analytics.track('user_created', { id: user.id });

  return user;
}
```

### Good - Single responsibility methods

```javascript
function handleFormSubmit(formData) {
  const validationError = validateFormData(formData);
  if (validationError) return validationError;

  const sanitizedData = sanitizeFormData(formData);
  const user = createUser(sanitizedData);

  notifyNewUser(user);

  return user;
}

function validateFormData(formData) {
  if (!formData.name) return createError('Name required');
  if (!formData.email) return createError('Email required');
  return null;
}

function sanitizeFormData(formData) {
  return {
    name: formData.name.trim(),
    email: formData.email.toLowerCase(),
  };
}

function createUser(data) {
  return db.users.create(data);
}

function notifyNewUser(user) {
  emailService.sendWelcome(user.email);
  analytics.track('user_created', { id: user.id });
}
```

# Naming Conventions and Patterns

## Naming Conventions

### Variables and Functions

- Use camelCase for variables and functions in JS/TS/Java/Kotlin
- Use snake_case for variables and functions in Python/Rust/Ruby/PHP
- Boolean variables should be prefixed with `is`, `has`, `should`, etc.
- Avoid abbreviations unless widely understood

### Classes and Types

- Use PascalCase for class names and type names
- Interface names: prefer no prefix (not `IUserService`, just `UserService`)
- Enum names: PascalCase for enum, SCREAMING_SNAKE_CASE for values

### Constants

- Use SCREAMING_SNAKE_CASE for true constants
- Group related constants in enums or objects

## Architectural Patterns

### Project Structure

- Organize by feature/domain, not by technical layer
- Keep related code close together
- Separate concerns clearly

### Error Handling

- Use Result/Either types for expected errors
- Use exceptions only for truly exceptional circumstances
- Always provide context in error messages
- Log errors at appropriate levels

### Configuration

- Use environment variables for environment-specific config
- Use config files for application defaults
- Never commit secrets or credentials
- Document all configuration options

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
const active = users.filter((u) => u.status === 1).map((u) => u.id);

// Simple and clear
const activeUserIds = users.filter((user) => user.isActive).map((user) => user.id);
```

## Start with the Straightforward Solution

1. **First: Write the naive, obvious solution**
2. **Then: Only optimize if there's a proven problem**
3. **Measure before optimizing** - don't guess at performance bottlenecks

```javascript
// Start here - simple and works
function findUser(users, id) {
  return users.find((user) => user.id === id);
}

// Only move to this if you've measured and proven the above is too slow
function findUser(users, id) {
  const userMap = new Map(users.map((u) => [u.id, u]));
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
  format(date, pattern) {
    /* custom parsing logic */
  }
}

// Use the platform
date.toISOString();
date.toLocaleDateString();
new Intl.DateTimeFormat('en-US').format(date);
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
  findById(id) {
    return this.db.users.findOne({ id });
  }
  findAll() {
    return this.db.users.find();
  }
  create(data) {
    return this.db.users.insert(data);
  }
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
const users = [
  /* user objects */
];
const user = users.find((u) => u.id === targetId);
```

## Remember

- **Complexity is the enemy of reliability**
- **Simple code is easier to test, debug, and modify**
- **Future you will thank present you for choosing simplicity**
- **When in doubt, do the simplest thing that could possibly work**
