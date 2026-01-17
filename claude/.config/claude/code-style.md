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
  return "Active";
}

// Good
const STATUS_ACTIVE = 2;
if (status === STATUS_ACTIVE) {
  return "Active";
}

// Even better - use enums/objects
const UserStatus = {
  INACTIVE: 1,
  ACTIVE: 2,
  SUSPENDED: 3
};
if (status === UserStatus.ACTIVE) {
  return "Active";
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

## Examples

### Bad - Large method with nested conditionals
```javascript
function processUser(user) {
  if (user) {
    if (user.active) {
      if (user.email) {
        return sendEmail(user.email);
      } else {
        return logError("No email");
      }
    } else {
      return logError("User inactive");
    }
  } else {
    return logError("No user");
  }
}
```

### Good - Early returns and extracted methods
```javascript
function processUser(user) {
  if (!user) return logError("No user");
  if (!user.active) return logError("User inactive");
  if (!user.email) return logError("No email");

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
  if (!formData.name) return createError("Name required");
  if (!formData.email) return createError("Email required");
  return null;
}

function sanitizeFormData(formData) {
  return {
    name: formData.name.trim(),
    email: formData.email.toLowerCase()
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
