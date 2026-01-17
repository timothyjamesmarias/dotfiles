# Naming Conventions and Patterns

## Naming Conventions

### Files and Directories

- Use kebab-case for file names: `user-service.ts`, `data-processor.py`
- Group related files in directories by feature/domain, not by type
- Test files: `*.test.ts`, `*_test.py`, `*_test.go`, `*_spec.rb`, `*Test.php`

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
