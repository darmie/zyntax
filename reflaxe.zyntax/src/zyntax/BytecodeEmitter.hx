package zyntax;

#if (macro || zyntax_runtime)

import haxe.io.Bytes;
import haxe.io.BytesOutput;
import haxe.Json;
import haxe.crypto.Crc32;
import zyntax.HIR;
import zyntax.AST;

/**
 * Emit Zyntax bytecode (.zbc) from HIR
 *
 * For now, uses JSON payload format which the Rust deserializer supports.
 * Future: Implement Postcard binary format for better performance.
 */
class BytecodeEmitter {
    static inline var MAGIC: Int = 0x5A424300; // "ZBC\0"
    static inline var MAJOR_VERSION: Int = 1;
    static inline var MINOR_VERSION: Int = 0;
    static inline var FORMAT_JSON: Int = 1;

    public static function emit(module: HirModule): Bytes {
        // Convert HIR to JSON payload
        var jsonObj = moduleToJson(module);
        var jsonStr = Json.stringify(jsonObj);
        var payload = Bytes.ofString(jsonStr);

        // Calculate CRC32 checksum
        var checksum = Crc32.make(payload);

        // Create header using bincode layout:
        // - All integers are little-endian
        // - Struct fields are written sequentially with no padding unless explicitly aligned
        var header = new BytesOutput();
        header.bigEndian = false;  // Explicitly set little-endian for bincode compatibility

        // Write magic number: 0x5A424300 ("ZBC\0") in little-endian
        // Little-endian means least significant byte first
        // 0x5A424300 = [0x00, 0x43, 0x42, 0x5A] in little-endian
        header.writeByte(0x00);  // '\0'
        header.writeByte(0x43);  // 'C'
        header.writeByte(0x42);  // 'B'
        header.writeByte(0x5A);  // 'Z'

        // Write version numbers (u16 little-endian)
        header.writeByte(MAJOR_VERSION & 0xFF);      // major low byte
        header.writeByte((MAJOR_VERSION >> 8) & 0xFF); // major high byte
        header.writeByte(MINOR_VERSION & 0xFF);      // minor low byte
        header.writeByte((MINOR_VERSION >> 8) & 0xFF); // minor high byte

        header.writeByte(FORMAT_JSON);               // u8: format
        // Bincode adds 3 bytes of padding here to align u32 flags
        header.writeByte(0);
        header.writeByte(0);
        header.writeByte(0);

        // Write flags (u32 little-endian)
        header.writeByte(0);
        header.writeByte(0);
        header.writeByte(0);
        header.writeByte(0);
        writeUuid(header, module.id);                // [u8; 16]: module_id

        // Write payload_size (u64 little-endian)
        var payloadSize = payload.length;
        header.writeByte(payloadSize & 0xFF);
        header.writeByte((payloadSize >> 8) & 0xFF);
        header.writeByte((payloadSize >> 16) & 0xFF);
        header.writeByte((payloadSize >> 24) & 0xFF);
        header.writeByte(0);  // High 32 bits (always 0 for small payloads)
        header.writeByte(0);
        header.writeByte(0);
        header.writeByte(0);

        // Write checksum (u32 little-endian)
        header.writeByte(checksum & 0xFF);
        header.writeByte((checksum >> 8) & 0xFF);
        header.writeByte((checksum >> 16) & 0xFF);
        header.writeByte((checksum >> 24) & 0xFF);

        // Combine header and payload
        var output = new BytesOutput();
        output.write(header.getBytes());
        output.write(payload);

        return output.getBytes();
    }

    static function moduleToJson(m: HirModule): Dynamic {
        return {
            id: m.id,
            name: m.name,
            functions: mapToObject(m.functions, functionToJson),
            globals: mapToObject(m.globals, globalToJson),
            types: {},  // HashMap<TypeId, HirType> - empty for now
            imports: m.imports,
            exports: m.exports,
            version: m.version,
            dependencies: []  // HashSet<HirId> - array representation
        };
    }

    static function functionToJson(f: HirFunction): Dynamic {
        return {
            id: f.id,
            name: f.name,
            signature: signatureToJson(f.signature),
            entry_block: f.entry_block,
            blocks: mapToObject(f.blocks, blockToJson),
            locals: {},  // HashMap<HirId, HirLocal> - empty for now
            values: mapToObject(f.values, valueToJson),
            previous_version: null,  // Option<HirId> - for hot-reloading
            is_external: f.is_external,
            calling_convention: callingConventionToString(f.calling_convention),
            attributes: {
                "inline": "None",  // InlineHint
                "no_return": false,
                "no_unwind": false,
                "no_inline": false,
                "always_inline": false,
                "cold": false,
                "hot": false,
                "pure": false,
                "const_fn": false
            }
        };
    }

    static function valueToJson(v: HirValue): Dynamic {
        return {
            id: v.id,
            ty: typeToJson(v.ty),
            kind: valueKindToJson(v.kind),
            uses: v.uses,
            span: v.span
        };
    }

    static function valueKindToJson(kind: HirValueKind): Dynamic {
        return switch (kind) {
            case Constant(c): {"Constant": constantToJson(c)};
            case Parameter(idx): {"Parameter": idx};
            case Instruction: "Instruction";
            case Global(id): {"Global": id};
            case Undef: "Undef";
        };
    }

    static function constantToJson(c: HirConstant): Dynamic {
        return switch (c) {
            case Bool(val): {"Bool": val};
            case I8(val): {"I8": val};
            case I16(val): {"I16": val};
            case I32(val): {"I32": val};
            case I64(val): {"I64": val};
            case I128(val): {"I128": val};
            case U8(val): {"U8": val};
            case U16(val): {"U16": val};
            case U32(val): {"U32": val};
            case U64(val): {"U64": val};
            case U128(val): {"U128": val};
            case F32(val): {"F32": val};
            case F64(val): {"F64": val};
            case String(val): {"String": val};
        };
    }

    static function signatureToJson(s: HirSignature): Dynamic {
        return {
            params: s.params.map(p -> ({
                id: p.id,
                name: p.name,
                ty: typeToJson(p.ty),
                attributes: p.attributes
            })),
            returns: s.returns.map(typeToJson),
            type_params: [],  // Vec<HirTypeParam> - empty for now
            const_params: [],  // Vec<HirConstParam> - empty for now
            lifetime_params: [],  // Vec<HirLifetime> - empty for now
            is_variadic: s.is_variadic,
            is_async: s.is_async
        };
    }

    static function blockToJson(b: HirBasicBlock): Dynamic {
        return {
            id: b.id,
            label: b.label,
            phis: b.phis.map(phiToJson),
            instructions: b.instructions.map(instructionToJson),
            terminator: terminatorToJson(b.terminator),
            dominance_frontier: [],  // HashSet<HirId> - empty for now
            predecessors: [],  // Vec<HirId> - will be computed by compiler
            successors: []  // Vec<HirId> - will be computed by compiler
        };
    }

    static function phiToJson(p: HirPhi): Dynamic {
        return {
            result: p.result,
            ty: typeToJson(p.ty),
            incoming: p.incoming.map(i -> [i.value, i.block])
        };
    }

    static function instructionToJson(inst: HirInstruction): Dynamic {
        return switch (inst) {
            case Binary(op, result, ty, left, right):
                {
                    Binary: {
                        op: binaryOpToString(op),
                        result: result,
                        ty: typeToJson(ty),
                        left: left,
                        right: right
                    }
                };

            case Call(result, callee, args, type_args, is_tail):
                {
                    Call: {
                        result: result,
                        callee: callableToJson(callee),
                        args: args,
                        type_args: type_args.map(typeToJson),
                        const_args: [],
                        is_tail: is_tail
                    }
                };

            case Alloca(result, ty, count, align):
                {
                    Alloca: {
                        result: result,
                        ty: typeToJson(ty),
                        count: count,
                        align: align
                    }
                };

            case Load(result, ty, ptr, align, is_volatile):
                {
                    Load: {
                        result: result,
                        ty: typeToJson(ty),
                        ptr: ptr,
                        align: align,
                        is_volatile: is_volatile
                    }
                };

            case Store(value, ptr, align, is_volatile):
                {
                    Store: {
                        value: value,
                        ptr: ptr,
                        align: align,
                        is_volatile: is_volatile
                    }
                };
        };
    }

    static function terminatorToJson(term: HirTerminator): Dynamic {
        return switch (term) {
            case Return(values):
                {Return: {values: values}};

            case Branch(target):
                {Branch: {target: target}};

            case CondBranch(condition, true_target, false_target):
                {
                    CondBranch: {
                        condition: condition,
                        true_target: true_target,
                        false_target: false_target
                    }
                };

            case Unreachable:
                "Unreachable";
        };
    }


    static function globalToJson(g: HirGlobal): Dynamic {
        return {
            id: g.id,
            name: g.name,
            ty: typeToJson(g.ty),
            initializer: g.initializer != null ? constantToJson(g.initializer) : null,
            is_const: g.is_const,
            is_thread_local: g.is_thread_local,
            linkage: linkageToString(g.linkage),
            visibility: visibilityToString(g.visibility)
        };
    }

    static function callableToJson(c: HirCallable): Dynamic {
        return switch (c) {
            case Function(id):
                {Function: id};

            case Indirect(value):
                {Indirect: value};
        };
    }

    static function typeToJson(t: HirType): Dynamic {
        return switch (t) {
            case Void: "Void";
            case Bool: "Bool";
            case I8: "I8";
            case I16: "I16";
            case I32: "I32";
            case I64: "I64";
            case I128: "I128";
            case U8: "U8";
            case U16: "U16";
            case U32: "U32";
            case U64: "U64";
            case U128: "U128";
            case F32: "F32";
            case F64: "F64";
            case Ptr(inner):
                {Ptr: typeToJson(inner)};
        };
    }

    static function binaryOpToString(op: BinaryOp): String {
        return switch (op) {
            case Add: "Add";
            case Sub: "Sub";
            case Mul: "Mul";
            case Div: "Div";
            case Rem: "Rem";
            case And: "And";
            case Or: "Or";
            case Xor: "Xor";
            case Shl: "Shl";
            case Shr: "Shr";
            case Eq: "Eq";
            case Ne: "Ne";
            case Lt: "Lt";
            case Le: "Le";
            case Gt: "Gt";
            case Ge: "Ge";
        };
    }

    static function callingConventionToString(cc: CallingConvention): String {
        return switch (cc) {
            case Cdecl: "C";
            case Rust: "Fast";  // Map Rust to Fast (internal functions)
            case Fast: "Fast";
            case System: "System";
        };
    }

    static function linkageToString(l: Linkage): String {
        return switch (l) {
            case Private: "Private";
            case Public: "Public";
            case External: "External";
        };
    }

    static function visibilityToString(v: Visibility): String {
        return switch (v) {
            case Default: "Default";
            case Hidden: "Hidden";
            case Protected: "Protected";
        };
    }

    static function writeUuid(output: BytesOutput, uuid: String) {
        // Parse UUID string (xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx) and write as 16 bytes
        var hex = uuid.split("-").join("");
        for (i in 0...16) {
            var byteHex = hex.substr(i * 2, 2);
            output.writeByte(Std.parseInt("0x" + byteHex));
        }
    }

    static function mapToArray<K, V>(map: Map<K, V>, converter: V -> Dynamic): Array<Dynamic> {
        var result = [];
        for (value in map) {
            result.push(converter(value));
        }
        return result;
    }

    static function mapToObject<K, V>(map: Map<K, V>, converter: V -> Dynamic): Dynamic {
        var result: Dynamic = {};
        for (key => value in map) {
            Reflect.setField(result, Std.string(key), converter(value));
        }
        return result;
    }
}


#end
