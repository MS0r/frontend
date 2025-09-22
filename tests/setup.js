vi.mock('monaco-editor', () => ({
  editor: {
    create: vi.fn(() => ({
      getValue: vi.fn(() => ''),
      setValue: vi.fn(),
      dispose: vi.fn(),
    })),
  },
  languages: {
    register: vi.fn(),
    setMonarchTokensProvider: vi.fn(),
  },
}))