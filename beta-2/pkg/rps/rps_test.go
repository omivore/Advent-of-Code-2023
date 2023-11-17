package rps

import "testing"

func TestHello(t *testing.T) {
    got := Hello("Test")
    if got != "Hi, Test. Welcome!" {
        t.Errorf("Hello(\"Test\") = %s, want \"Hi, Test. Welcome!\"", got)
    }
}
