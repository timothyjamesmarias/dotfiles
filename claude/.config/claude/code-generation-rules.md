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
