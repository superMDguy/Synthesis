
typedef union
#ifdef __cplusplus
	YYSTYPE
#endif
	{
			int					i;
			char				*str;
			struct objport_t	obj;
			struct Agnode_t		*n;
} YYSTYPE;
extern YYSTYPE aglval;
# define T_graph 257
# define T_digraph 258
# define T_subgraph 259
# define T_strict 260
# define T_node 261
# define T_edge 262
# define T_edgeop 263
# define T_symbol 264
