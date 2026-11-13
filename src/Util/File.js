// FFI for Util.File.

// Copies the files out of the input, then clears it, in that order. See
// `takeFiles` for why the order is fixed here.
export const takeFilesImpl = (input) => () => {
  const files = Array.from(input.files ?? []);
  input.value = "";
  return files;
};

export const arrayBufferImpl = (file) => () => file.arrayBuffer();
