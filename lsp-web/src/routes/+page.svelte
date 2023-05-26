<script lang="ts">
  import JsonLdCode from "$lib/components/JsonLdCode.svelte";

  let value = `
{
  "@context": "https://raw.githubusercontent.com/json-ld/json-ld.org/main/contexts/person.jsonld",
  "@id": "http://example.com/me",
  "knows": {"@id": "http://example.com/you"}
}
`;
</script>

<JsonLdCode bind:value />
