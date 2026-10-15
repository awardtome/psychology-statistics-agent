// Bound model payloads without pretending a preview is the complete result.
function modelContext(value, limit = 150) {
  const truncated = [];
  function copy(item, path = '$') {
    if (Array.isArray(item)) {
      if (item.length > limit) truncated.push({path, total: item.length, shown: limit});
      return item.slice(0, limit).map((v, i) => copy(v, `${path}[${i}]`));
    }
    if (item && typeof item === 'object') {
      return Object.fromEntries(Object.entries(item)
        .filter(([key]) => !['image', 'images', 'file', 'files', 'figureImage'].includes(key))
        .map(([key, v]) => [key, copy(v, `${path}.${key}`)]));
    }
    return item;
  }
  return {...copy(value), contextTruncation: truncated,
    contextNote: truncated.length ? '大表仅为预览；完整结果在本地报告中，不能把截取部分当全部结果。' : ''};
}
module.exports = {modelContext};
