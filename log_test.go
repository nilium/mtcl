package mtcl

import "testing"

// logf is a pointer to the current test's Logf function.
// Only used for debugging.
var logf = func(string, ...interface{}) {}

func setlogf(t *testing.T) {
	temp := logf
	logf = t.Logf
	t.Cleanup(func() { logf = temp })
}
