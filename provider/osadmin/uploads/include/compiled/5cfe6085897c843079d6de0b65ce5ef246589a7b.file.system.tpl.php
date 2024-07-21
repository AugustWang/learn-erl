<?php /* Smarty version Smarty-3.1.15, created on 2014-11-13 13:02:16
         compiled from "E:\software\wamp\www\osadmin\uploads\include\template\panel\system.tpl" */ ?>
<?php /*%%SmartyHeaderCode:3164454643b58311c82-38427333%%*/if(!defined('SMARTY_DIR')) exit('no direct access allowed');
$_valid = $_smarty_tpl->decodeProperties(array (
  'file_dependency' => 
  array (
    '5cfe6085897c843079d6de0b65ce5ef246589a7b' => 
    array (
      0 => 'E:\\software\\wamp\\www\\osadmin\\uploads\\include\\template\\panel\\system.tpl',
      1 => 1382249386,
      2 => 'file',
    ),
  ),
  'nocache_hash' => '3164454643b58311c82-38427333',
  'function' => 
  array (
  ),
  'variables' => 
  array (
    'sys_info' => 0,
  ),
  'has_nocache_code' => false,
  'version' => 'Smarty-3.1.15',
  'unifunc' => 'content_54643b583b5db7_82878658',
),false); /*/%%SmartyHeaderCode%%*/?>
<?php if ($_valid && !is_callable('content_54643b583b5db7_82878658')) {function content_54643b583b5db7_82878658($_smarty_tpl) {?><?php echo $_smarty_tpl->getSubTemplate ("header.tpl", $_smarty_tpl->cache_id, $_smarty_tpl->compile_id, 0, null, array(), 0);?>

<?php echo $_smarty_tpl->getSubTemplate ("navibar.tpl", $_smarty_tpl->cache_id, $_smarty_tpl->compile_id, 0, null, array(), 0);?>

<?php echo $_smarty_tpl->getSubTemplate ("sidebar.tpl", $_smarty_tpl->cache_id, $_smarty_tpl->compile_id, 0, null, array(), 0);?>

<!-- TPLSTART 以上内容不需更改，保证该TPL页内的标签匹配即可 -->
<div class="block">
	<a href="#page-stats" class="block-heading" data-toggle="collapse">系统信息</a>
	<div id="page-stats" class="block-body collapse in">
	
	 <table class="table table-striped">
              <tbody>							  
					<tr><td>服务器时间</td><td><?php echo $_smarty_tpl->tpl_vars['sys_info']->value['gmt_time'];?>
 (格林威治标准时间)</td></tr>
					<tr><td>服务器时间</td><td><?php echo $_smarty_tpl->tpl_vars['sys_info']->value['bj_time'];?>
 (北京时间)</td></tr>
					<tr><td>服务器ip地址</td><td><?php echo $_smarty_tpl->tpl_vars['sys_info']->value['server_ip'];?>
</td></tr>
					<tr><td>服务器解译引擎</td><td><?php echo $_smarty_tpl->tpl_vars['sys_info']->value['software'];?>
</td></tr>
					<tr><td>web服务端口</td><td><?php echo $_smarty_tpl->tpl_vars['sys_info']->value['port'];?>
</td></tr>
					<tr><td>Mysql 版本</td><td><?php echo $_smarty_tpl->tpl_vars['sys_info']->value['mysql_version'];?>
</td></tr>
					<tr><td>服务器管理员</td><td><?php echo $_smarty_tpl->tpl_vars['sys_info']->value['admin'];?>
</td></tr>
					<tr><td>服务端剩余空间</td><td><?php echo $_smarty_tpl->tpl_vars['sys_info']->value['diskfree'];?>
</td></tr>
					<tr><td>系统当前用户名</td><td><?php echo $_smarty_tpl->tpl_vars['sys_info']->value['current_user'];?>
</td></tr>
					<tr><td>系统时区</td><td><?php echo $_smarty_tpl->tpl_vars['sys_info']->value['timezone'];?>
</td></tr>
              </tbody>
     </table>  																		
								 
	</div>
</div>
<!-- TPLEND 以下内容不需更改，请保证该TPL页内的标签匹配即可 -->
<?php echo $_smarty_tpl->getSubTemplate ("footer.tpl", $_smarty_tpl->cache_id, $_smarty_tpl->compile_id, 0, null, array(), 0);?>
<?php }} ?>
